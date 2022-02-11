path <- "path.xls"
test <-tibble(sheet = excel_sheets(path)) %>%
  mutate(
    has_header = sheet == "Sheet1",
    data = sheet %>% map2(has_header, ~ {
      if (.y) {
        read_excel(path, sheet = .x, skip = 3)
      } else {
        read_excel(path, sheet = .x, col_names = c("Date", "Category", "CatID", "Revenue"))
      }
    })
  ) %>%
  pull(data) %>%
  bind_rows()
