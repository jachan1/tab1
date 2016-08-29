tab1_fxn_hpr <- function(ds, tab_in){
  # if(tab_in$var=="ddx") browser()
  var_values <- ds[[as.character(tab_in$var)]]
  if(class(var_values) == "factor") {
    targets <- levels(var_values)
    var_values <- as.character(var_values)
  } else {
    var_values <- ifelse(var_values == "", NA, var_values)
    targets <- unique(var_values[!is.na(var_values)])
  }
  n_avail <- sum(!is.na(var_values))
  pct_fxn <- function(target=tab_in$target) sprintf("%1.1f%% (%g)", 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T))
  
  value = if(tab_in$type == "c"){
    sprintf("%1.1f (%1.1f)", mean(var_values, na.rm=T), sd(var_values, na.rm=T))
  } else if(tab_in$type == "b"){
    pct_fxn()
  }
  if(tab_in$type == "m"){
    values <- sapply(targets, pct_fxn)
    data_frame(group=tab_in$varnm, Characteristic=targets, N=n_avail, `Percent (n) or Mean (SD)`=values)
  } else {
    data_frame(group=tab_in$group, Characteristic=tab_in$varnm, N=n_avail, `Percent (n) or Mean (SD)`=value)
  }
}

tab1_fxn <- function(tab_in, ds, grp){
  # print(tab_in)
  # if(tab_in$var=="ddx") browser()
  ## tab_in should include columns "varnm", "var", "type", "group"
  ## if type == "b" then target should be available
  # if(tab_in$var == "white") browser()
  if(!missing(grp)){
    ds %>% group_by_(grp) %>% do(tab1_fxn_hpr(.,tab_in))
  } else {
    ds %>% do(tab1_fxn_hpr(., tab_in))
  }
}

grp_tirc <- function(x, rgroup_col="group", grp="study_grp", rnames="Characteristic"){
  if(!rgroup_col %in% names(x)) x[[rgroup_col]]=""
  cols <- setdiff(names(x), grp)
  # print(cols)
  wide <- Reduce(function(x, y) merge(x, y, by=c(rgroup_col, rnames), all=T), lapply(unique(x[[grp]]), function(y) x[x[[grp]]==y, cols]))
  grps <- unique(x[[grp]])
  names(wide) <- c(rgroup_col, rnames, rep(setdiff(cols, c(rnames, rgroup_col)), length(grps)))
  TIRC(wide, rnames=rnames, rgroup_col=rgroup_col, cgroup=grps, n.cgroup=rep(length(cols)-2, length(grps)))
}

# tab1 <- bind_rows(data_frame(varnm='Age (yrs)', var='age', group='', type='c'),
#                   data_frame(varnm='White', var='race', group='', type='b', target=4),
#                   data_frame(varnm='Not Hispanic or Latino', var='ethnicity', group='', type='b', target=1),
#                   data_frame(varnm='Female', var='gender', group='', type='b', target=0),
#                   data_frame(varnm='FamHx ALS', var='fhals', group='', type='b', target=1),
#                   data_frame(varnm='Disease Duration', var='dis_dur', group='', type='c'),
#                   data_frame(varnm='Diagnostic Delay', var='diag_delay', group='', type='c'),
#                   data_frame(varnm='Diagnosis', var='ddx', group='', type='m'),
#                   data_frame(varnm='el_escorial', var='el_escorial', group='', type='m'),
#                   data_frame(varnm='Site Of Onset', var='site_of_onset', group='', type='m')) %>% 
#   group_by(ordr=1:n()) %>% do(tab1_fxn(., demo, grp="arm")) %>% ungroup
# grp_tirc(tab1 %>% select(-ordr), grp="arm")