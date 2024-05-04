test_that("make_info_myu", {
  # Conditions
  victim <- c("Father", "Father", "Mother", "Mother", "Son", "Son", "Daughter", "Daughter",
              "Brother", "Brother", "Sister", "Sister",
              "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle",
              "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
              "Nephew", "Niece", "Nephew", "Niece",
              "Nephew", "Niece", "Nephew", "Niece",
              "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather",
              "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother",
              "Grandson", "Granddaughter", "Grandson", "Granddaughter",
              "Grandson", "Granddaughter", "Grandson", "Granddaughter")

  reference <- c("Son", "Daughter", "Son", "Daughter", "Father", "Mother", "Father", "Mother",
                 "Brother", "Sister", "Brother", "Sister",
                 "Nephew", "Niece", "Nephew", "Niece",
                 "Nephew", "Niece", "Nephew", "Niece",
                 "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle",
                 "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
                 "Grandson", "Granddaughter", "Grandson", "Granddaughter",
                 "Grandson", "Granddaughter", "Grandson", "Granddaughter",
                 "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather",
                 "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother")

  name <- mapply(paste, victim, reference, sep = "_")

  pibd2 <- c(0, 0, 0, 0, 0, 0, 0, 0,
             0.25, 0.25, 0.25, 0.25,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0,
             0, 0, 0, 0)

  pibd1 <- c(1, 1, 1, 1, 1, 1, 1, 1,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5)

  pibd0 <- c(0, 0, 0, 0, 0, 0, 0, 0,
             0.25, 0.25, 0.25, 0.25,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5,
             0.5, 0.5, 0.5, 0.5)

  paternal <- c("Yes", "No", "No", "No", "Yes", "No", "No", "No",
                "Yes", "No", "No", "No",
                "Yes", "No", "No", "No",
                "No", "No", "No", "No",
                "Yes", "No", "No", "No",
                "No", "No", "No", "No",
                "Yes", "No", "No", "No",
                "No", "No", "No", "No",
                "Yes", "No", "No", "No",
                "No", "No", "No", "No")

  maternal <- c("No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes",
                "Yes", "Yes", "Yes", "Yes",
                "No", "No", "Yes", "Yes",
                "No", "No", "Yes", "Yes",
                "No", "No", "Yes", "Yes",
                "No", "No", "Yes", "Yes",
                "No", "No", "No", "No",
                "No", "No", "Yes", "Yes",
                "No", "No", "No", "No",
                "No", "No", "Yes", "Yes")

  tree_persons <- c("Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1",
                    "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2",
                    "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                    "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                    "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                    "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                    "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                    "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                    "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                    "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3")

  tree_sexes <- c("M, M, F", "M, F, F", "F, M, M", "F, F, M", "M, M, F", "M, F, M", "F, M, F", "F, F, M",
                  "M, M, M, F", "M, F, M, F", "F, M, M, F", "F, F, M, F",
                  "M, M, M, F, M, F", "M, F, M, F, M, F", "M, M, M, F, F, M", "M, F, M, F, F, M",
                  "F, M, M, F, M, F", "F, F, M, F, M, F", "F, M, M, F, F, M", "F, F, M, F, F, M",
                  "M, M, M, F, M, F", "F, M, M, F, M, F", "M, M, M, F, F, M", "F, M, M, F, F, M",
                  "M, F, M, F, M, F", "F, F, M, F, M, F", "M, F, M, F, F, M", "F, F, M, F, F, M",
                  "M, M, F, M, F", "M, F, F, M, F", "M, M, F, F, M", "M, F, F, F, M",
                  "F, M, M, M, F", "F, F, M, M, F", "F, M, M, F, M", "F, F, M, F, M",
                  "M, M, F, M, F", "F, M, F, M, F", "M, M, F, F, M", "F, M, F, F, M",
                  "M, F, M, M, F", "F, F, M, M, F", "M, F, M, F, M", "F, F, M, F, M")

  tree_fathers <- c("0, Victim, 0", "0, Victim, 0", "0, UK1, 0", "0, UK1, 0", "Ref, 0, 0", "UK1, 0, 0", "Ref, 0, 0", "UK1, 0, 0",
                    "UK1, UK1, 0, 0", "UK1, UK1, 0, 0", "UK1, UK1, 0, 0", "UK1, UK1, 0, 0",
                    "UK1, UK3, 0, 0, UK1, 0", "UK1, UK3, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0",
                    "UK1, UK3, 0, 0, UK1, 0", "UK1, UK3, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0",
                    "UK3, UK1, 0, 0, UK1, 0", "UK3, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0",
                    "UK3, UK1, 0, 0, UK1, 0", "UK3, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0",
                    "0, UK2, 0, Victim, 0", "0, UK2, 0, Victim, 0", "0, UK3, 0, Victim, 0", "0, UK3, 0, Victim, 0",
                    "0, UK2, 0, UK1, 0", "0, UK2, 0, UK1, 0", "0, UK3, 0, UK1, 0", "0, UK3, 0, UK1, 0",
                    "UK2, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0",
                    "UK2, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0")

  tree_mothers <- c("0, UK1, 0", "0, UK1, 0", "0, Victim, 0", "0, Victim, 0", "UK1, 0, 0", "Ref, 0, 0", "UK1, 0, 0", "Ref, 0, 0",
                    "UK2, UK2, 0, 0", "UK2, UK2, 0, 0", "UK2, UK2, 0, 0", "UK2, UK2, 0, 0",
                    "UK2, UK4, 0, 0, UK2, 0", "UK2, UK4, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0",
                    "UK2, UK4, 0, 0, UK2, 0", "UK2, UK4, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0",
                    "UK4, UK2, 0, 0, UK2, 0", "UK4, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0",
                    "UK4, UK2, 0, 0, UK2, 0", "UK4, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0",
                    "0, UK3, 0, UK1, 0", "0, UK3, 0, UK1, 0", "0, UK2, 0, UK1, 0", "0, UK2, 0, UK1, 0",
                    "0, UK3, 0, Victim, 0", "0, UK3, 0, Victim, 0", "0, UK2, 0, Victim, 0", "0, UK2, 0, Victim, 0",
                    "UK3, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0",
                    "UK3, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0")

  tree_founders <- c("Yes, No, Yes", "Yes, No, Yes", "Yes, No, Yes", "Yes, No, Yes", "No, Yes, Yes", "No, Yes, Yes", "No, Yes, Yes", "No, Yes, Yes",
                     "No, No, Yes, Yes", "No, No, Yes, Yes", "No, No, Yes, Yes", "No, No, Yes, Yes",
                     "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                     "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                     "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                     "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                     "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes",
                     "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes",
                     "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes",
                     "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes")

  dt_rel <- data.table(Relationship = name,
                       Pr_IBD2 = pibd2, Pr_IBD1 = pibd1, Pr_IBD0 = pibd0,
                       Paternal = paternal, Maternal = maternal,
                       Tree_persons = tree_persons, Tree_sexes = tree_sexes, Tree_fathers = tree_fathers, Tree_mothers = tree_mothers, Tree_founders = tree_founders)

  # Run
  tmp <- make_info_myu(dt_rel)
  bool_pc_all <- tmp$bool_pc_all
  bool_parent_victim_all <- tmp$bool_parent_victim_all
  bool_parent_male_all <- tmp$bool_parent_male_all

  # Test
  expect_equal(length(bool_pc_all), 44)
  expect_equal(all(bool_pc_all[1:8]), TRUE)
  expect_equal(any(bool_pc_all[9:44]), FALSE)
  expect_equal(length(bool_parent_victim_all), 44)
  expect_equal(all(bool_parent_victim_all[1:4]), TRUE)
  expect_equal(any(bool_parent_victim_all[5:44]), FALSE)
  expect_equal(length(bool_parent_male_all), 44)
  expect_equal(all(bool_parent_male_all[c(1, 2, 5, 7)]), TRUE)
  expect_equal(any(bool_parent_male_all[c(3, 4, 6, 8, 9:44)]), FALSE)
})
