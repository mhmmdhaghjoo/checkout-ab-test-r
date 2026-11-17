for (file in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}
