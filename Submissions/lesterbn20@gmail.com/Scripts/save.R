#save items for rmd

saveRDS(
  list(
    p_alignment_v_coverage = p_alignment_v_coverage,
    pgrid_routes = pgrid_routes,
    rc_soe = rc_soe,
    rc_cov_soe = rc_cov_soe
  ),
  "for_rmd.rds"
)

