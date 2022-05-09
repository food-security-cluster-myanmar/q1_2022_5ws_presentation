
# this is to produce the kable table at the end 
fsc %>%  
  group_by(admin3_pcode_old) %>% 
  summarise(beneficiaries = sum(new_beneficiaries),
            partners = n_distinct(org_code)) %>% 
  right_join(pin, by = c("admin3_pcode_old" = "admin3_pcode")) %>% 
  replace_na(list(beneficiaries = 0)) %>% 
  mutate(reached_pc = beneficiaries / fs_targeted,
         reached_pc = ifelse(is.infinite(reached_pc), 1, reached_pc),
         reached_pc = round(reached_pc * 100, digits = 2), 
         fs_targeted = round(fs_targeted, digits = 0), 
         overreach = beneficiaries - fs_targeted) %>% 
  arrange(desc(overreach)) %>% 
  select(state, township, target = fs_targeted, beneficiaries, overreach, `%_reached` = reached_pc, partners) %>% 
  filter(overreach > 0 & `%_reached` >= 100 & target != 0) %>% 
  kable(caption = "Most oversubscribed townships", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped") %>% 
  kable_styling(font_size = 20) %>% 
  save_kable(file = "oversubscribed_townships.png", zoom = 2)


library(xaringanBuilder)
# building pdf version of slides
build_pdf("q1_presentation.Rmd")
