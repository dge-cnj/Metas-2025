# Processamento de dados do STJ

# 07/04/2025: Verificação e teste

# Meta 2 de 2025 – Julgar processos mais antigos. 
# Julgar, até 31/12/2025, 100% dos processos distribuídos até 31/12/2018.

STJ.Meta02 <- function(dge) {
    
    t_f <- "2026-01-01"
    t_0 <- "2019-01-01"
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_a = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    return(dge)

}

# Meta 4 de 2025 – Priorizar o julgamento dos processos relativos aos crimes contra a administração pública, à improbidade administrativa e aos ilícitos eleitorais.
# - Julgar, até 31/12/2025, 90% das ações de improbidade administrativa e das ações penais relacionadas a crimes contra a Administração Pública distribuídas até 31/12/2023.
# - Identificar e julgar até 26/10/2025, 100% das ações de improbidade administrativa distribuídas até 26/10/2021.

STJ.Meta04 <- function(dge) {

    t_f <- "2026-01-01"
    t_0 <- "2024-01-01"
    b_f <- "2025-10-27" # Final do período "b"
    b_0 <- "2021-10-27" # Início do período "b"
    
    # 1ª parte
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm4_a_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism4_a = if_else((flg_imp == "TRUE" | flg_crim_contr_adm_pbl == "TRUE") & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_a =="1") & (baixm4_a_ate == 0 | is.na(baixm4_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_a = if_else(dism4_a == "1" & (primeirasentm4_a == "1" | decm4_a == "1" | baixm4_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_a = if_else(dism4_a == "1" & (julgadom4_a == "0" | is.na(julgadom4_a)) & pendente_meta == "0", 1, 0))
    
    # 2ª parte
    
    dge <- dge %>% 
        mutate(
            primeirasentm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_julgamento_sem_pronuncia >= b_0 & data_total_primeiro_julgamento_sem_pronuncia < b_f, 1, 0),
            baixm4_b = if_else((flg_imp == "TRUE") & data_total_primeira_baixa >= b_0 & data_total_primeira_baixa < b_f, 1, 0),
            baixm4_b_ate = if_else(data_total_primeira_baixa < b_0 | data_total_primeiro_julgamento_sem_pronuncia < b_0 | data_total_primeiro_procedimento_resolvido < b_0, 1, 0),
            decm4_b = if_else((flg_imp == "TRUE") & data_total_primeiro_procedimento_resolvido >= b_0 & data_total_primeiro_procedimento_resolvido < b_f, 1, 0),
            dism4_b = if_else((flg_imp == "TRUE") & dt_recebimento < b_0 & (data_total_primeiro_julgamento_sem_pronuncia >= b_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm4_b =="1") & (baixm4_b_ate == 0 | is.na(baixm4_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom4_b = if_else(dism4_b == "1" & (primeirasentm4_b == "1" | decm4_b == "1" | baixm4_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm4_b = if_else(dism4_b == "1" & (julgadom4_b == "0" | is.na(julgadom4_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)
    
}

# Meta 6 de 2025 – Priorizar o julgamento das ações ambientais.
# Julgar, até 31/12/2025, 75% dos processos relacionados às ações ambientais distribuídos até 31/12/2024.

STJ.Meta06 <- function(dge) {
    
    t_f <- "2026-01-01"
    t_0 <- "2025-01-01"
    
    dge <- dge %>%
        mutate(
            primeirasentm6_a = if_else(flg_amb == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm6_a = if_else(flg_amb == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm6_a_ate = if_else(flg_amb == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm6_a = if_else(flg_amb == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism6_a = if_else(flg_amb == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm6_a =="1") & (baixm6_a_ate == 0 | is.na(baixm6_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom6_a = if_else(dism6_a == "1" & (primeirasentm6_a == "1" | decm6_a == "1" | baixm6_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm6_a = if_else(dism6_a == "1" & (julgadom6_a == "0" | is.na(julgadom6_a)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Meta 7 de 2025 – Priorizar o julgamento dos processos relacionados aos indígenas e quilombolas.
# Julgar, até 31/12/2025, 75% dos processos relacionados aos direitos das comunidades indígenas e 75% dos processos relacionados aos direitos das comunidades quilombolas distribuídos até 31/12/2024. 

STJ.Meta07 <- function(dge) {

    
    t_f <- "2026-01-01"
    t_0 <- "2025-01-01"
    
    # Comunidades Indígenas
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_a = if_else(flg_ind == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_a = if_else(flg_ind == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_a_ate = if_else(flg_ind == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_a = if_else(flg_ind == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_a = if_else(flg_ind == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_a =="1") & (baixm7_a_ate == 0 | is.na(baixm7_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_a = if_else(dism7_a == "1" & (primeirasentm7_a == "1" | decm7_a == "1" | baixm7_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_a = if_else(dism7_a == "1" & (julgadom7_a == "0" | is.na(julgadom7_a)) & pendente_meta == "0", 1, 0))
    
    # Quilombolas
    
    dge <- dge %>% 
        mutate(
            primeirasentm7_b = if_else(flg_qui == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm7_b = if_else(flg_qui == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm7_b_ate = if_else(flg_qui == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm7_b = if_else(flg_qui == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism7_b = if_else(flg_qui == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm7_b =="1") & (baixm7_b_ate == 0 | is.na(baixm7_b_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom7_b = if_else(dism7_b == "1" & (primeirasentm7_b == "1" | decm7_b == "1" | baixm7_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm7_b = if_else(dism7_b == "1" & (julgadom7_b == "0" | is.na(julgadom7_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Meta 8 de 2025 – Priorizar o julgamento dos processos relacionados ao feminicídio e à violência doméstica e familiar contra as mulheres.
# Julgar, até 31/12/2025, 100% dos casos de feminicídio e de violência doméstica e familiar contra a mulher distribuídos até 31/12/2023.

STJ.Meta08 <- function(dge) {
    
    t_f <- "2026-01-01"
    t_0 <- "2024-01-01"
    
    #Violência Doméstica
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_a_ate = if_else(flag_violencia_domestica == "1" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0), 1, 0),
            decm8_a = if_else(flag_violencia_domestica == "1" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f | data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism8_a = if_else(flag_violencia_domestica == "1" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia)), 1, 0))
    dge <- dge %>% mutate(julgadom8_a = if_else(dism8_a == "1" & (primeirasentm8_a == "1" | decm8_a == "1" | baixm8_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_a = if_else(dism8_a == "1" & (julgadom8_a == "0" | is.na(julgadom8_a)) & pendente_meta == "0", 1, 0))
    
    #Feminicídio (a seguir, flag_feminicidio comparado com tRUEfoi trocado por flag_fem)
    
    dge <- dge %>% 
        mutate(
            primeirasentm8_b = if_else(flag_feminicidio == "1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm8_b = if_else(flag_feminicidio == "1" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm8_b_ate = if_else(flag_feminicidio == "1" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_f), 1, 0),
            decm8_b = if_else(flag_feminicidio == "1" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism8_b = if_else(flag_feminicidio == "1" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia)), 1, 0))
    dge <- dge %>% mutate(julgadom8_b = if_else(dism8_b == "1" & (primeirasentm8_b == "1" | decm8_b == "1" | baixm8_b == "1"), 1, 0))
    dge <- dge %>% mutate(susm8_b = if_else(dism8_b == "1" & (julgadom8_b == "0" | is.na(julgadom8_b)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Meta 10 de 2025 – Promover os direitos da criança e do adolescente.
# Julgar 100% dos casos de sequestro internacional de crianças, distribuídos até 31/12/2024.

STJ.Meta10 <- function(dge) {

    
    t_f <- "2026-01-01"
    t_0 <- "2025-01-01"
    
    dge <- dge %>% 
        mutate(
            primeirasentm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm10_a = if_else(flg_seq == "TRUE" & data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm10_a_ate = if_else(flg_seq == "TRUE" & (data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0), 1, 0),
            decm10_a = if_else(flg_seq == "TRUE" & data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism10_a = if_else(flg_seq == "TRUE" & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm10_a =="1") & (baixm10_a_ate == 0 | is.na(baixm10_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom10_a = if_else(dism10_a == "1" & (primeirasentm10_a == "1" | decm10_a == "1" | baixm10_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm10_a = if_else(dism10_a == "1" & (julgadom10_a == "0" | is.na(julgadom10_a)) & pendente_meta == "0", 1, 0))
    
    return (dge)

}

# Hub principal, para o STJ

ProcessarDados.STJ <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do STJ"))

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo ", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        pre_processual <- temp$pre_processual
    
        dge <- Determinar.Meta01(dge)
        dge <- STJ.Meta02(dge)
        dge <- STJ.Meta04(dge)
        dge <- STJ.Meta06(dge)
        dge <- STJ.Meta07(dge)
        dge <- STJ.Meta08(dge)
        dge <- STJ.Meta10(dge)
    
        GerarArquivos(dge, x)
        
        rm (temp, pre_processual, dge)
        gc()

    } # Fim do for (i in  seq_along(aux))

    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
