rm(list=ls())

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

n_sis = 25    # n siswa
n_har = 20    # banyak hari
max_cap = 10  # max kelas
min_cap = 5   # min kelas
min_frek = 4  # min frek
max_frek = 12  # max frek


hasil = 
  MIPModel() %>%
  # menambah variabel
  add_variable(x[i,j],
               i = 1:n_sis,
               j = 1:n_har,
               type = "binary",
               lb = 0) %>%
  # membuat objective function
  set_objective(sum_expr(x[i,j],
                         i = 1:n_sis,
                         j = 1:n_har),
                "max") %>%
  # menambah constraints
  # max kapasitas kelas
  add_constraint(sum_expr(x[i,j],i = 1:n_sis) >= min_cap,
                 j = 1:n_har) %>%
  add_constraint(sum_expr(x[i,j],i = 1:n_sis) <= max_cap,
                 j = 1:n_har) %>%
  # frek kunjungan siswa
  add_constraint(sum_expr(x[i,j],j = 1:n_har) >= min_frek,
                 i = 1:n_sis) %>%
  add_constraint(sum_expr(x[i,j],j = 1:n_har) <= max_frek,
                 i = 1:n_sis) %>%
  # jeda sehari
  add_constraint(x[i,j] + x[i,j+1] <= 1,
                 i = 1:n_sis,
                 j = 1:(n_har-1)) %>%
  solve_model(with_ROI(solver = "glpk",
                       verbose = T))

rekap = 
  hasil %>% 
  get_solution(x[i,j]) %>%
  filter(value == 1) %>%
  rename(siswa = i,
         hari = j)

rekap %>%
  group_by(hari) %>%
  summarise(presensi = paste(siswa,collapse = ",")) %>%
  ungroup() %>%
  knitr::kable("simple",caption = "Jadwal Kunjungan Siswa")

rekap %>%
  group_by(siswa) %>%
  tally() %>%
  ungroup() %>%
  rename("jumlah kehadiran" = n) %>%
  knitr::kable("simple",caption = "Rekap Presensi Siswa")


library(ggplot2)
rekap %>% 
  ggplot(aes(x = as.factor(hari), 
             y = as.factor(siswa))) +
  geom_tile()
