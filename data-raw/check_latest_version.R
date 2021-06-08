check_latest_version <- function(syn_id, version) {
  syn_info <- synapser::synGet(syn_id, downloadFile = FALSE)
  latest_version <- syn_info[["properties"]][["versionNumber"]]

  if (latest_version != version) {
    usethis::ui_todo("{syn_id} is not up to date - using version {version} but latest is {latest_version}. Please update version used.")
  }
}
