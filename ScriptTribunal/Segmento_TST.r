
# Processamento de dados do Tribunal Superior do Trabalho

# 18/09/2025: Validação da versão paralelizada.

# Meta 2 de 2025 – Julgar processos mais antigos



TST.Meta02 <- function(dge) {
    
    t_0 <- "2025-01-01" # Início do período
    t_f <- "2026-01-01" # Final do período de cômputo da meta 02
    t_dist <- "2020-01-01" # Data de corte para um processo ser considerado na meta 02
    
    t_rec <- "2020-01-01" ### Verificar esta data
    
    # Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate = if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_rec & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & desm1 == "1", 1, 0))
    
    return(dge)

}

# Meta 3 de 2025 – Estimular a Conciliação

# Aumentar o índice de conciliação na fase de conhecimento em 0,5 ponto percentual em relação à média do biênio 2022/2023 ou alcançar, no mínimo, 38% de conciliação.

TST.Meta03 <- function() {

}

# Meta 5 de 2025 – Reduzir a taxa de congestionamento

# Reduzir em 0,5 ponto percentual a taxa de congestionamento líquida, exceto execuções fiscais, em relação a 2024.

TST.Meta05 <- function() {

}

# Meta 10 de 2025 – Promover os direitos da criança e do adolescente

# Promover, no âmbito do Programa de Combate ao Trabalho Infantil e Estímulo à Aprendizagem, pelo menos uma ação de combate ao trabalho infantil e de estímulo à aprendizagem, preferencialmente, voltada à promoção da equidade racial, de gênero ou diversidade do público-alvo, por meio do estabelecimento de parcerias interinstitucionais.

TST.Meta10 <- function() {

}

# Hub principal, para o TST

ProcessarDados.TST <- function(path, aux, x) {
  
    RegistrarLOG(paste0("Processando dados do TST"))

    for (i in  seq_along(aux)){
      
        RegistrarLOG(paste0("Dados do arquivo", aux[i]))
        
        temp <- iniciarFlags(path, aux, i)
        dge <- temp$dge
        dge <- Determinar.Meta01(dge)
        dge <- TST.Meta02(dge)
    
        GerarArquivos(dge, x)

    } # Fim do for (i in  seq_along(aux))

    RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))       

}
