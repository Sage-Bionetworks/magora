list_tpm_files <- function(id) {
  files <- as.list(synapser::synGetChildren(id))

  # Filter only to include files with suffix "htseq_count.txt"
  files %>%
    purrr::transpose() %>%
    dplyr::as_tibble() %>%
    dplyr::select(name, id) %>%
    tidyr::unnest(cols = c(name, id)) %>%
    dplyr::filter(stringr::str_ends(name, "htseq_count.txt"))
}

download_tpm_and_annotations <- function(tpm_files, folder) {
  tpm_list <- list(id = tpm_files[["id"]], name = tpm_files[["name"]], version = tpm_files[["version"]])

  # Pull and save data from synapse
  # Save then rename to synapse ID so we can connect to the annotation data later on without having to pull each more than once (e.g. for iterating on code)
  purrr::pmap(
    tpm_list,
    function(id, name, version) {
      synapser::synGet(id, version = version, downloadLocation = here::here("data-raw", "gene_expressions", folder))
      fs::file_move(here::here("data-raw", "gene_expressions", folder, name), here::here("data-raw", "gene_expressions", folder, paste0(id, ".txt")))
    }
  )

  # Pull and save annotation data
  purrr::walk(tpm_files[["id"]], function(x) {
    synapser::synGetAnnotations(x) %>%
      saveRDS(here::here("data-raw", "gene_expressions", folder, paste0(x, ".rds")))
  })

  # Save IDs for reading in files

  saveRDS(tpm_files["id"], here::here("data-raw", "gene_expressions", folder, "ids.rds"))
}

read_combine_tpm_and_annotations <- function(folder) {

  # Read in IDS
  ids <- readRDS(here::here("data-raw", "gene_expressions", folder, "ids.rds"))

  # Read in tpm

  tpm <- ids %>%
    dplyr::mutate(data = purrr::map(id, function(x) {
      read.table(here::here("data-raw", "gene_expressions", folder, paste0(x, ".txt")), header = FALSE) %>%
        dplyr::as_tibble()
    })) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::rename(gene_id = V1, value = V2)

  # Read in annotations and pull IDs
  mouse_ids <- ids %>%
    dplyr::mutate(data = map(id, function(x) {
      annotations <- readRDS(here::here("data-raw", "gene_expressions", folder, paste0(x, ".rds")))
      dplyr::tibble(
        specimen_id = annotations[["specimenID"]][[1]],
        mouse_id = annotations[["individualID"]][[1]]
      )
    })) %>%
    tidyr::unnest(cols = c(data))

  # Combine data
  tpm %>%
    dplyr::left_join(mouse_ids, by = "id") %>%
    dplyr::select(-id)
}
