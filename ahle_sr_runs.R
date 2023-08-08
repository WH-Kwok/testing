ahle_sr_runs <- function(
    file="D:/Users/SK/Documents/gbad/AHLE scenario parameters-20221202.xlsx",
    sheet = 1, par_names = "AHLE Parameter",
    ignore = "Notes", nruns = 5, Num_months = 12, 
    baseline_scen = "CLM_S_Current", control_scen = NULL, ill_month = NULL
){
  lapply( c("tidyverse", "readxl", "freedom", "truncnorm","data.table", "doParallel", "abind"), require, character.only = T)
  cl = (detectCores() *.94) %>% floor %>% makeCluster
  registerDoParallel(cl)
  age = c("N", "J", "A"); sex = c("F", "M"); cats = c(age, sex) 
  .g = expand_grid(age,sex)
  .t =  paste0(.g$age,.g$sex)      
  .p = paste0(".", .t) %>% as.list
  .l = map(cats, function(x) map(.t, function(y) grepl(x, y) %>% sum))
  names(.l) <- cats %>% tolower %>% paste0(".", .)
  data = read_excel(file, sheet)
  if(is.null(baseline_scen)){
    baseline_scen = names(data) %>% . [ !. %in% c(par_names, ignore)] 
  } else baseline_scen = baseline_scen
  f_scen = function(scen){
    if(is.null(scen)) par = NULL else {
      par = select(data, all_of(c(par_names, scen))) %>% transpose(., make.names = par_names) %>%
        . [ , (colSums(is.na(.))/nrow(.)) < .5] %>% split( . , seq(nrow(.))) %>% 
        map(. , ~ map(.x, ~ eval(parse(text = .x)))) %>% 
        map(. , 
            function(x, name){
              attach(x)
              N0 = list(N_NF_t0, N_NM_t0, N_JF_t0, N_JM_t0, N_AF_t0, N_AM_t0)
              names(N0) <- name
              par = list(
                g = Beta, 
                pt = part, 
                pf = prolif, 
                pfm =	prop_F_milked, 
                ld = lac_duration, 
                ady =	avg_daily_yield_ltr, 
                mvl =	milk_value_ltr, 
                o = list(GammaF, GammaM) %>% rep( 3 ), 
                d	= list(AlphaN, AlphaN, AlphaJ, AlphaJ, AlphaF, AlphaM), 
                c = list(CullF, CullM) %>% rep( 3 ), 
                lw = list(lwNF, lwNM, lwJF, lwJM,lwAF,lwAM), 
                ccy =	ccy, 
                fv = list(fvNF, fvNM, fvJF, fvJM, fvAF, fvAM), 
                hr = hides_rate, 
                hrm =	hides_rate_mor, 
                hv = hides_value, 
                m = list( Man_N, Man_J, Man_A) %>% rep( each = 2), 
                mv = Man_value, 
                dm = list(DM_req_prpn_NF, DM_req_prpn_NM, DM_req_prpn_JF, DM_req_prpn_JM, DM_req_prpn_AF, DM_req_prpn_AM), 
                plp =	prpn_lskeepers_purch_feed, 
                pfp =	prpn_feed_paid_for, 
                fc = Feed_cost_kg, 
                dif =	DM_in_feed, 
                lsr =	Lab_SR, 
                lnh =	lab_non_health, 
                he = Health_exp, 
                ir = Interest_rate, 
                iph	=	Infrastructure_per_head
              )
              detach(x)
              par %>% map( ~ {
                if( is.list(.x) ) .x = .x else .x = list(.x) %>% rep(6)
              } ) %>% c(list(N0 = N0), .)
            } , .p) 
    }; par
  } 
  b_scen = f_scen(baseline_scen)
  c_scen = f_scen(control_scen)
  f_par = function(
    b_scen, c_scen, age, sex, cats, .g, .t, .p, .l, data,
    file, sheet, par_names, ignore, nruns, Num_months, baseline_scen, control_scen, ill_month
  ){
    out = foreach(seed = 1:nruns) %dopar% {
      require(tidyverse)  
      pop <- list()
      pop[[1]] <- b_scen$N0
      for(i in 1:Num_months){
        attach(pop[[i]])
        pre = list(.AF, .AF, .NF, .NM, .JF, .JM)
        post = list(.NF, .NM, .JF, .JM, .AF, .AM)
        detach(pop[[i]])
        if(i %in% ill_month) p = c_scen else p = b_scen 
        pop[[ i+1 ]] <- pmap(
          list(pre = pre, post = post, .t = .t, .p = .p) %>% c(., .l, p), 
          function(pre, post, .t, .p, .n, .j, .a, .f, .m, N0, g, pt, pf, pfm, ld, ady, mvl, 
                   o, d, c, lw, ccy, fv, hr, hrm, hv, m, mv, dm, plp, pfp, fc, dif, lsr, lnh, he, ir, iph){
            set.seed(seed)
            s = function(x) sample(x, 1)
            df = pmap( list( 
              list(pt*pf/12/2, g, g, d, o, c) %>% map(s), 
              c(list(pre) %>% rep(2), list(post) %>% rep(4)),
              list(.n, .j+.a, .n+.j, 1, .j+.a, .a)
            ), \(x,y,z) x*y*z) %>% data.frame
            names(df) <- c("B", "Gi", "Go", "D", "O", "C")
            Df = df %>% mutate(
              sum = rowSums(.), 
              come = B + Gi, 
              leave = sum - come, 
              diff = come - leave, 
              new = post + diff, 
              NOftk = O * (.j + .a) + C * (.a * .m), 
              PopGro = diff, 
              TotNChg = NOftk + PopGro, 
              TotMort = D, 
              QLwKg = new *s(lw), 
              OftkLw = NOftk *s(lw), 
              QMeat = OftkLw *ccy, 
              QManu = new *s(m) *30, 
              QHides = D * (.j + .a) * hrm, 
              QMilk = new * (.a * .f) * s(pt) * pfm * ld * ady, 
              QWool = 0, 
              CumDM = new * dm * s(lw) * 30, 
              ValOftk = NOftk *s(fv), 
              ValHerd = diff *s(fv), 
              TotVal = ValHerd + ValOftk, 
              ValManu = QManu * mv, 
              ValHides = QHides * hv, 
              ValMilk = QMilk * mvl, 
              ProdVal = TotVal + ValManu + ValHides + ValMilk, 
              FdCost = new * dm * s(lw) * plp * pfp/s(dif) * s(fc) * 30, 
              LbCost = new * s(lsr) * lnh, 
              HthCost = new * s(he), 
              CapCost = ifelse(i == 1, new, 0) * s(fv) * ir, 
              IstCost = ifelse(i == 1, N0, 0) * s(iph), 
              TotExp = FdCost + LbCost + HthCost + CapCost + IstCost, 
              GrsMrg = ProdVal - TotExp, 
              update = new
            ) 
            names(Df) = names(Df) [ - length(Df) ] %>% paste0(., "_", .t) %>% c(., .p); Df
          }) %>% data.frame  
      }
      out = pop %>% .[-1] %>% do.call(rbind, .) 
      prf = names(out) [ 1 : (length(out)/length(.t)) ] %>% gsub( .t [[1]] , "", .)
      tags = c("", "Overall", paste0(cats, "Com")) %>% map(., ~ paste0(prf, .x))
      subtot = pmap(tags, 
                    function(p, o, n, j, a, f, m){
                      df = out %>% select(starts_with(p)) 
                      rs = rowSums(df) 
                      mp = map(.l, function(x) apply(df, 1, function(y) (unlist(x) * y) %>% sum)) %>% data.frame
                      DF = data.frame(df, rs, mp) 
                      names(DF) [ - c(1 : length(.t)) ] <- c(o, n, j, a, f, m)
                      DF
                    }) %>% data.frame 
      xc = subtot %>% select(starts_with(c(".", "new", "QLwKg"))) %>% names
      subtot %>% mutate_if( ! names(.) %in% xc, cumsum)
    }
    f=function(x){
      y <- names(x)
      z <- y %>%
        gsub("B_","Births_",.)%>%
        gsub("Gi_","Growth in_",.)%>%
        gsub("Go_","Growth out_",.)%>%
        gsub("D_","Deaths_",.)%>%
        gsub("O_","Offtakes_",.)%>%
        gsub("C_","Culls_",.)%>%
        gsub("sum_","Sum_",.)%>%
        gsub("come_","Inflow_",.)%>%
        gsub("leave_","Outflow_",.)%>%
        gsub("diff_","Difference_",.)%>%
        gsub("new_","Population_",.)%>%
        gsub("NOftk_","Num Offtake_",.)%>%
        gsub("PopGro_","Cml Pop Growth_",.)%>%
        gsub("TotNChg_","Total Number Increase_",.)%>%
        gsub("TotMort_","Total Mortality_",.)%>%
        gsub("QLwKg_","Population Liveweight (kg)_",.)%>%
        gsub("OftkLw_","Offtake Liveweight (kg)_",.)%>%
        gsub("QMeat_","Meat (kg)_",.)%>%
        gsub("QManu_","Manure_",.)%>%
        gsub("QHides_","Hides_",.)%>%
        gsub("QMilk_","Milk_",.)%>%
        gsub("QWool_","Wool_",.)%>%
        gsub("CumDM_","Cml Dry Matter_",.)%>%
        gsub("ValOftk_","Value of Offtake_",.)%>%
        gsub("ValHerd_","Value of Herd Increase_",.)%>%
        gsub("TotVal_","Value of Herd Increase plus Offtake_",.)%>%
        gsub("ValManu_","Value of Manure_",.)%>%
        gsub("ValHides_","Value of Hides_",.)%>%
        gsub("ValMilk_","Value of Milk_",.)%>%
        gsub("ProdVal_","Total Production Value_",.)%>%
        gsub("FdCost_","Feed Cost_",.)%>%
        gsub("LbCost_","Labour Cost_",.)%>%
        gsub("HthCost_","Health Cost_",.)%>%
        gsub("CapCost_","Capital Cost_",.)%>%
        gsub("IstCost_","Infrastructure Cost_",.)%>%
        gsub("TotExp_","Total Expenditure_",.)%>%
        gsub("GrsMrg_","Gross Margin_",.)%>%
        gsub("NF_","Neonatal Female_",.)%>%
        gsub("NM_","Neonatal Male_",.)%>%
        gsub("JF_","Juvenile Female_",.)%>%
        gsub("JM_","Juvenile Male_",.)%>%
        gsub("AF_","Adult Female_",.)%>%
        gsub("AM_","Adult Male_",.)%>%
        gsub("Overall_","Overall_",.)%>%
        gsub("NCom_","Neonatal Combined_",.)%>%
        gsub("JCom_","Juvenile Combined_",.)%>%
        gsub("ACom_","Adult Combined_",.)%>%
        gsub("FCom_","Female Combined_",.)%>%
        gsub("MCom_","Male Combined_",.)
      names(x) <- z
      x       
    }
    map(out,f)
  }
  output =map(b_scen, f_par, c_scen[[1]], age, sex, cats, .g, .t, .p, .l, data, 
              file, sheet, par_names, ignore, nruns, Num_months, baseline_scen, control_scen, ill_month)
  names(output) = baseline_scen
  stopCluster(cl)
  registerDoSEQ()
  output
}