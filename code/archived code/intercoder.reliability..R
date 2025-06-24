# install.packages(c("readxl","dplyr","purrr","openxlsx"))
library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)

setwd("C:/Nall Research Group Dropbox/Clayton Nall/niskanen/code")
# 1) point to your file
file_in  <- "../data/Housing Bills 2023 to 2024.xlsx"
file_out <- "../data/Augmented_Data_2009_to_2024.xlsx"

# 2) read master sheet
master <- read_excel(file_in, sheet = "Data 2009 to 2024")

# 3) identify which sheets are the coder‐sheets
all_sheets <- excel_sheets(file_in)
skip      <- c("Data 2009 to 2024",
               "Coding Example","Coding Sheet",
               "Codebook","Examples","Compiled Data")
coder_sheets <- setdiff(all_sheets, skip)

# 4) pull out the coder assignments
assignments <- map_dfr(coder_sheets, function(sh) {
  df <- read_excel(file_in, sheet = sh, col_types = "text")
  df %>% select(bill_id = `Housing Bill`, coder1, coder2)
})

# 5) pull out each RA’s codings (only those three cols: bill, coder2, billtype, overlay)
codings <- map_dfr(coder_sheets, function(sh) {
  df <- read_excel(file_in, sheet = sh, col_types = "text")
  df %>% select(bill_id = `Housing Bill`,
                coder_name = coder2,
                billtype    = `Bill Type`,
                overlay     = Overlay)
})

# 6) keep each coder‐pair only once (we only need coder1 < coder2 in text order)
assignments_unique <- assignments %>%
  filter(coder1 < coder2) %>%
  distinct()

# 7) join in coder2’s values…
pairs <- assignments_unique %>%
  left_join(codings %>%
              rename(coder2 = coder_name,
                     coder2_billtype = billtype,
                     coder2_overlay  = overlay),
            by = c("bill_id","coder2")) %>%
  # 8) …and coder1’s values
  left_join(codings %>%
              rename(coder1 = coder_name,
                     coder1_billtype = billtype,
                     coder1_overlay  = overlay),
            by = c("bill_id","coder1")) %>%
  # 9) dummy flags for agreement
  mutate(billtype_match = as.integer(coder1_billtype == coder2_billtype),
         overlay_match  = as.integer(coder1_overlay  == coder2_overlay))

# 10) plumb it onto your master data
augmented <- master %>%
  left_join(pairs,
            by = c("Housing Bill" = "bill_id"))

# 11) write out the new workbook
write.xlsx(augmented, file_out, overwrite = TRUE)

message("Done — saved to ", file_out)

