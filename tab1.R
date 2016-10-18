tab1_fxn_hpr <- function(ds, tab_in, pp, mp){
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
  pct_fxn <- function(target=tab_in$target) sprintf("%1.*f%% (%g)", pp, 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T))
  
  value = if(tab_in$type == "c"){
    sprintf("%1.*f (%1.*f)", mp, mean(var_values, na.rm=T), mp, sd(var_values, na.rm=T))
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

test_grp <- function(ds, grp, tab_in, sub="referenceid"){
  fm1 <- formula(sprintf("%s ~ %s", tab_in$var, grp))
  if(tab_in$type == "c"){
    lm1 <- lm(fm1, data=na.omit(ds[c(tab_in$var, grp, sub)]))
    slm1 <- summary(lm1)
    anova(lm1)[1, "Pr(>F)"]
  } else {
    chisq.test(ds[[tab_in$var]], ds[[grp]])$p.value
  }
}

tab1_fxn <- function(tab_in, ds, grp, pp=1, mp=1, test=F){
  # print(tab_in)
  # if(tab_in$var=="ddx") browser()
  ## tab_in should include columns "varnm", "var", "type", *optional* "group"
  ## if type == "b" then target should be available
  # if(tab_in$var == "white") browser()
  if(!"group" %in% names(tab_in)) tab_in$group = ""
  if(!missing(grp)){
    ds_out <- ds %>% group_by_(grp) %>% do(tab1_fxn_hpr(.,tab_in, pp=pp, mp=mp))
    if(test){
      p <- tryCatch(test_grp(ds, grp, tab_in), error=function(e) NA)
      ds_out <- ds_out %>% ungroup %>% mutate(p=p)
    }
    ds_out
  } else {
    ds %>% do(tab1_fxn_hpr(., tab_in, pp=pp, mp=mp))
  }
}

grp_tirc <- function(x, rgroup_col="group", grp="study_grp", rnames="Characteristic", p=F){
  if(p == F) p <- as.character()
  if(!rgroup_col %in% names(x)) x[[rgroup_col]]=""
  cols <- setdiff(names(x), c(grp, p))
  # print(cols)
  wide <- Reduce(function(x, y) merge(x, y, by=c(rgroup_col, rnames), all=T), lapply(unique(x[[grp]]), function(y) x[x[[grp]]==y, cols]))
  if(length(p) > 0) wide <- wide %>% left_join(unique(x[c(rnames, p)]), by=rnames)
  names(wide) <- c(rgroup_col, rnames, rep(setdiff(cols, c(rnames, rgroup_col)), length(unique(x[[grp]]))), p)
  grps <- as.character(unique(x[[grp]]))
  ngrps <- rep(length(cols)-2, length(grps))
  if(length(p) > 0) {
    grps <- c(grps, "")
    ngrps <- c(ngrps, 1)
    wide[[p]] <- round(wide[[p]], 3)
  }
  TIRC(wide, rnames=rnames, rgroup_col=rgroup_col, cgroup=grps, n.cgroup=ngrps)
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