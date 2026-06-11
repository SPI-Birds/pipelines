if (!exists("data_path")) {
    data_path <- choose_directory()
}

withr::defer(teardown_env())
