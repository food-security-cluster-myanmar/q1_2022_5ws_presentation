
fsc %>%  
  group_by(admin3_pcode_old) %>% 
  summarise(beneficiaries = sum(new_beneficiaries),
            partners = n_distinct(org_code)) %>% 
  right_join(pin, by = c("admin3_pcode_old" = "admin3_pcode")) %>% 
  replace_na(list(beneficiaries = 0)) %>% 
  mutate(reached_pc = beneficiaries / fs_targeted,
         reached_pc = ifelse(is.infinite(reached_pc), 1, reached_pc),
         reached_pc = round(reached_pc * 100, digits = 2), 
         fs_targeted = ifelse(fs_targeted == 0 & beneficiaries > 0, 1, fs_targeted),
         fs_targeted = round(fs_targeted, digits = 0), 
         gap = fs_targeted - beneficiaries) %>% 
  arrange(gap) %>% 
  select(state, township, target = fs_targeted, beneficiaries, gap, `%_reached` = reached_pc, partners) %>% 
  filter(gap < 0 & `%_reached` >= 100) %>% 
  mutate(target = ifelse(target == 1, 0, target), 
         gap = ifelse(target == 0, gap - 1, gap)) %>% 
  kable(caption = "Most oversubscribed townships", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped") %>% 
  save_kable(file = "oversubscribed_townships.png", zoom = 2)

