Attribution_function <- function(
    `AHLE estimates` = AHLE ,
    `Expert attribution` = Att ,
    `Number of samples` = 1000 ,
    Variables = c("Species", "AHLE", "Production system","Age class", "Cause")  ,
    Parameters = c("mean", "sd", "min", "avg", "max")  ,  
    ...
){
  n <- `Number of samples`; p <- Parameters
  fn <- function(x) stats::rnorm(n, x[,p[1]], x[,p[2]])
  fp <- function(x)  mc2d::rpert(n, x[,p[3]], x[,p[4]], x[,p[5]])
  fd <- function(d, f){
    a <- d %>% select_if(names(.) %in% Variables)
    b <- d %>% select_if(names(.) %in% Parameters) %>% 
      mutate_all(function(x) if(mean(x)>1) x/100 else x)
    c <- cbind(a, b) # %>% group_by_at(names(a)) %>% summarise(across(names(b), base::mean), .groups = 'drop') %>% as.data.frame
    ca <- c %>% select(names(a))
    cb <- c %>% select(names(b)) %>%split(seq(nrow(.))) %>%
      lapply(f) %>%do.call(rbind,.) %>%as.data.frame
    return(list(ftr = names(ca), num = names(cb), df = cbind(ca, cb)))}
  fm <- function(x, y, z) x %>% melt(
    id.vars = y, variable.name = "Sample", value.name = z)
  fs <- function(x){
    Median = median(x); Mean = mean(x); SD = sd(x); SE = sd/sqrt(n); C = qnorm(0.975); 
    `95% CI Lower` = Mean - SE*C; `95% CI Upper` = Mean + SE*C; 
    data.frame(Median, Mean, SD, `95% CI Lower`, `95% CI Upper`)}
  A <- fd( as.data.frame(`AHLE estimates`) , fn)
  A_ <- fm(A$df, A$ftr, "Group_value")
  E <- fd( as.data.frame(`Expert attribution`) , fp) 
  E.df <-E$df %>%group_by_at(intersect(A$ftr, E$ftr)) %>%
    mutate(across(E$num, ~.x/sum(.x))) %>%as.data.frame; 
  E_ <- fm(E.df, E$ftr, "AFp")
  J <- left_join(E_, A_) %>%mutate(value = AFp*Group_value, .keep = "unused")
  out <- J %>% group_by_at(Variables) %>% summarise(fs(value), .groups = "drop") 
  return(list(`Summary statistics` = out, `Merged data` = J))
}
