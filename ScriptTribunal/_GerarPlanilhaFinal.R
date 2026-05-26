
Consolidado.Meta01 <- function(dge) {
    
    meta1 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mes_cnm1, mes_sent) %>% 
        summarise(
            casos_novos_2025 = sum(cnm1,na.rm = TRUE),
            julgados_2025 = sum(julgadom1,na.rm = TRUE),
            prim_sent2025 = sum(primeirasentm1, na.rm = TRUE),
            suspensos_2025 = sum(susm1,na.rm = TRUE),
            dessobrestados_2025 = sum(desm1,na.rm = TRUE),
            cumprimento_meta1 = (sum(julgadom1,na.rm = TRUE)/(sum(cnm1,na.rm = TRUE)+sum(desm1,na.rm = TRUE)-sum(susm1,na.rm = TRUE)))*100)  
    
    return(meta1)
    
}

Consolidado.STM <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/9.5),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/9.9),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/9.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/9.9))
    
    
    return (list(meta2=meta2, meta4=meta4))
    
}

Consolidado.JusticaMilitar <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/9),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/9.5),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/9.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/9.5))

    return (list(meta2=meta2, meta4=meta4))    
}

Consolidado.JusticaEleitoral <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/7),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),#### Verificar com o Pedro
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/9),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/5))
    
    return (list(meta2=meta2, meta4=meta4))
    
}

Consolidado.JusticaFederal <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/8.5),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/10),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),#### Verificar com o Pedro
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta3 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mes_senth) %>% 
        summarise(
            IC2024 = (sum(senth24,na.rm = TRUE)+sum(pre24,na.rm = TRUE))/sum(sent24,na.rm = TRUE),
            IC2025 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE))/sum(sent25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC2024+0.005),(IC2025/(IC2024+0.005))*100,if_else(IC2025>="0.08",100,(IC2025/(IC2024+0.005))*100)),
            quant_sent24 = sum(sent24,na.rm = TRUE),
            quant_conc24 = (sum(senth24,na.rm = TRUE)+sum(pre24,na.rm = TRUE)),
            quant_sent25 = sum(sent25,na.rm = TRUE),
            quant_conc25 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE)))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE) / (sum(dism4_a,na.rm = TRUE) - sum(susm4_a,na.rm = TRUE)))*(1000/7),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE) / (sum(dism4_b,na.rm = TRUE) - sum(susm4_b,na.rm = TRUE)))*(100))
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6")) fator <- 1000/2.5
    else fator <- 1000/3.5        # TRF2, 3, 4, 5
    
    meta6 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE) / (sum(dism6_a,na.rm = TRUE) - sum(susm6_a,na.rm = TRUE)))*fator)
    
    meta7 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a))*fator,
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),
            cumprimento_meta6c = (sum(julgadom7_b,na.rm = TRUE)/(sum(dism7_b,na.rm = TRUE)-sum(susm7_b,na.rm = TRUE)))*fator)
    
    meta10 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE) / (sum(dism10_a,na.rm = TRUE) - sum(susm10_a,na.rm = TRUE)))*(1000/10))

    
    return (list(meta2=meta2, meta3=meta3, meta4=meta4, meta6=meta6, meta7=meta7, meta10=meta10))    
    
}



Consolidado.JusticaEstadual <- function(dge) {

    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/8),
            distm2_b = sum(dism2_b,na.rm = TRUE),
            julgm2_b = sum(julgadom2_b,na.rm = TRUE),
            suspm2_b = sum(susm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgadom2_b,na.rm = TRUE)/(sum(dism2_b,na.rm = TRUE)-sum(susm2_b,na.rm = TRUE)))*(1000/9),
            distm2_c = sum(dism2_c,na.rm = TRUE),
            julgm2_c = sum(julgadom2_c,na.rm = TRUE),
            suspm2_c = sum(susm2_c,na.rm = TRUE),
            cumprimento_meta2c = (sum(julgadom2_c,na.rm = TRUE)/(sum(dism2_c,na.rm = TRUE)-sum(susm2_c,na.rm = TRUE)))*(1000/9.5),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),#### Verificar com o Pedro
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta3 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mes_senth) %>% 
        summarise(
            quant_sent24 = sum(sent24,na.rm = TRUE),
            quant_conc24 = (sum(senth24,na.rm = TRUE)+sum(pre24,na.rm = TRUE)),
            quant_sent25 = sum(sent25,na.rm = TRUE),
            quant_conc25 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE)),
            IC2024 = (sum(senth24,na.rm = TRUE)+sum(pre24,na.rm = TRUE))/sum(sent24,na.rm = TRUE),
            IC2025 = (sum(senth25,na.rm = TRUE)+sum(pre25,na.rm = TRUE))/sum(sent25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC2024+0.01),(IC2025/(IC2024+0.01))*100,if_else(IC2025>="0.17",100,(IC2025/(IC2024+0.01))*100)))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/6.5),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE) /(sum(dism4_b,na.rm = TRUE) - sum(susm4_b,na.rm = TRUE)))*(1000/10))
    
    meta6 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE) /(sum(dism6_a,na.rm = TRUE) - sum(susm6_a,na.rm = TRUE)))*(1000/5))
    
    meta7 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (sum(julgadom7_a,na.rm = TRUE)/(sum(dism7_a,na.rm = TRUE) - sum(susm7_a,na.rm = TRUE)))*(1000/5),
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),
            cumprimento_meta7b = (sum(julgadom7_b,na.rm = TRUE) /(sum(dism7_b,na.rm = TRUE) - sum(susm7_b,na.rm = TRUE)))*(1000/5))
    
    meta8 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm8_a = sum(dism8_a,na.rm = TRUE),
            julgm8_a = sum(julgadom8_a,na.rm = TRUE),
            suspm8_a = sum(susm8_a,na.rm = TRUE),
            cumprimento_meta8a = (sum(julgadom8_a,na.rm = TRUE)/(sum(dism8_a,na.rm = TRUE) - sum(susm8_a,na.rm = TRUE)))*(1000/9),
            distm8_b = sum(dism8_b,na.rm = TRUE),
            julgm8_b = sum(julgadom8_b,na.rm = TRUE),
            suspm8_b = sum(susm8_b,na.rm = TRUE),
            cumprimento_meta8b = (sum(julgadom8_b,na.rm = TRUE) /(sum(dism8_b,na.rm = TRUE) - sum(susm8_b,na.rm = TRUE)))*(1000/7.5))
    
    meta10 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE)/(sum(dism10_a,na.rm = TRUE)-sum(susm10_a,na.rm = TRUE)))*(1000/9),
            distm10_b = sum(dism10_b,na.rm = TRUE),
            julgm10_b = sum(julgadom10_b,na.rm = TRUE),
            suspm10_b = sum(susm10_b,na.rm = TRUE),
            cumprimento_meta10b = (sum(julgadom10_b,na.rm = TRUE)/(sum(dism10_b,na.rm = TRUE)-sum(susm10_b,na.rm = TRUE)))*(1000/10))
    
    return (list(meta2=meta2, meta3=meta3, meta4=meta4, meta6=meta6, meta7=meta7, meta8=meta8, meta10=meta10))    
    
}

Consolidado.JusticaTrabalho <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/9.4),
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))
    
    meta3 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome, mes_senth) %>% 
        summarise(
            IC22_23 = sum(senth22_23,na.rm = TRUE)/sum(sent22_23,na.rm = TRUE),
            IC2025 = sum(senth_25,na.rm = TRUE)/sum(sent_25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC22_23+0.005),(IC2025/(IC22_23+0.005))*100,if_else(IC2025>="0.38",100,(IC2025/(IC22_23+0.005))*100)),
            quant_sent22_23 = sum(sent22_23,na.rm = TRUE),
            quant_conc22_23 = sum(senth22_23,na.rm = TRUE),
            quant_sent25 = sum(sent_25,na.rm = TRUE),
            quant_conc25 = sum(senth_25,na.rm = TRUE))      
    
    return (list(meta2=meta2, meta3=meta3))    
    
} 

Consolidado.STJ <- function(dge) {
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_a = sum(dism2_a,na.rm = TRUE),
            julgm2_a = sum(julgadom2_a,na.rm = TRUE),
            suspm2_a = sum(susm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgadom2_a,na.rm = TRUE)/(sum(dism2_a,na.rm = TRUE)-sum(susm2_a,na.rm = TRUE)))*(1000/10))
    
    meta4 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm4_a = sum(dism4_a,na.rm = TRUE),
            julgm4_a = sum(julgadom4_a,na.rm = TRUE),
            suspm4_a = sum(susm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgadom4_a,na.rm = TRUE)/(sum(dism4_a,na.rm = TRUE)-sum(susm4_a,na.rm = TRUE)))*(1000/9),
            distm4_b = sum(dism4_b,na.rm = TRUE),
            julgm4_b = sum(julgadom4_b,na.rm = TRUE),
            suspm4_b = sum(susm4_b,na.rm = TRUE),                
            cumprimento_meta4b = (sum(julgadom4_b,na.rm = TRUE)/(sum(dism4_b,na.rm = TRUE)-sum(susm4_b,na.rm = TRUE)))*(1000/10))
    
    meta6 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm6_a = sum(dism6_a,na.rm = TRUE),
            julgm6_a = sum(julgadom6_a,na.rm = TRUE),
            suspm6_a = sum(susm6_a,na.rm = TRUE),
            cumprimento_meta6a = (sum(julgadom6_a,na.rm = TRUE)/(sum(dism6_a,na.rm = TRUE)-sum(susm6_a,na.rm = TRUE)))*(1000/7.5))
    
    meta7 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm7_a = sum(dism7_a,na.rm = TRUE),
            julgm7_a = sum(julgadom7_a,na.rm = TRUE),
            suspm7_a = sum(susm7_a,na.rm = TRUE),
            cumprimento_meta7a = (sum(julgadom7_a,na.rm = TRUE)/(sum(dism7_a,na.rm = TRUE)-sum(susm7_a,na.rm = TRUE)))*(1000/7.5),
            distm7_b = sum(dism7_b,na.rm = TRUE),
            julgm7_b = sum(julgadom7_b,na.rm = TRUE),
            suspm7_b = sum(susm7_b,na.rm = TRUE),                
            cumprimento_meta7 = (sum(julgadom7_b,na.rm = TRUE)/(sum(dism7_b,na.rm = TRUE)-sum(susm7_b,na.rm = TRUE)))*(1000/7.5))
    
    meta8 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm8_a = sum(dism8_a,na.rm = TRUE),
            julgm8_a = sum(julgadom8_a,na.rm = TRUE),
            suspm8_a = sum(susm8_a,na.rm = TRUE),
            cumprimento_meta8a = (sum(julgadom8_a,na.rm = TRUE)/(sum(dism8_a,na.rm = TRUE)-sum(susm8_a,na.rm = TRUE)))*(1000/10),
            distm8_b = sum(dism8_b,na.rm = TRUE),
            julgm8_b = sum(julgadom8_b,na.rm = TRUE),
            suspm8_b = sum(susm8_b,na.rm = TRUE),
            cumprimento_meta8b = (sum(julgadom8_b,na.rm = TRUE)/(sum(dism8_b,na.rm = TRUE)-sum(susm8_b,na.rm = TRUE)))*(1000/10))
    
    meta10 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm10_a = sum(dism10_a,na.rm = TRUE),
            julgm10_a = sum(julgadom10_a,na.rm = TRUE),
            suspm10_a = sum(susm10_a,na.rm = TRUE),
            cumprimento_meta10a = (sum(julgadom10_a,na.rm = TRUE)/(sum(dism10_a,na.rm = TRUE)-sum(susm10_a,na.rm = TRUE)))*(1000/10))
    
    return (list(meta2=meta2, meta4=meta4, meta6=meta6, meta7=meta7, meta8=meta8, meta10=meta10))    
    
}


Consolidado.TST <- function(dge) {
    
    colnames(dge)
    
    meta2 <- dge %>% 
        group_by(sigla_tribunal, procedimento, ramo_justica, sigla_grau, uf_oj, municipio_oj, id_ultimo_oj, nome) %>% 
        summarise(
            distm2_ant = sum(dism2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgadom2_ant,na.rm = TRUE),
            suspm2_ant = sum(susm2_ant,na.rm = TRUE),
            desom2_ant = sum(desm2_ant,na.rm = TRUE),#### Verificar com o Pedro
            cumprimento_meta2ant = (sum(julgadom2_ant,na.rm = TRUE)/(sum(dism2_ant,na.rm = TRUE)-sum(susm2_ant,na.rm = TRUE)))*(1000/10))

    return (list(meta2=meta2))
    
}

# ## Gerando a planilha final para alimentar o painel.



GerarPlanilhaFinal <- function() {

    Painel <- NULL
    
    STM <- c("STM")
    STJ <- c("STJ")
    TST <- c("TST")
    JusticaMilitar <- c("TJMMG", "TJMSP", "TJMRS")
    JusticaEleitoral <- c("TRE-AC", "TRE-AL", "TRE-AM", "TRE-AP", "TRE-BA", "TRE-CE", "TRE-DF", "TRE-ES", "TRE-GO", "TRE-MA", "TRE-MG", "TRE-MS", "TRE-MT", "TRE-PA", "TRE-PB", "TRE-PE", "TRE-PI", "TRE-PR", "TRE-RJ", "TRE-RN", "TRE-RO", "TRE-RR", "TRE-RS", "TRE-SC", "TRE-SE", "TRE-SP", "TRE-TO")
    JusticaEstadual <- c("TJAC", "TJAL", "TJAM", "TJAP", "TJBA", "TJCE", "TJDFT", "TJES", "TJGO", "TJMA", "TJMG", "TJMS", "TJMT", "TJPA", "TJPB", "TJPE", "TJPI", "TJPR", "TJRJ", "TJRN", "TJRO", "TJRR", "TJRS", "TJSC", "TJSE", "TJSP", "TJTO")
    JusticaFederal <- c("TRF1", "TRF2", "TRF3", "TRF4", "TRF5", "TRF6")
    JusticaTrabalho <- c("TRT1", "TRT2", "TRT3", "TRT4", "TRT5", "TRT6", "TRT7", "TRT8", "TRT9", "TRT10", "TRT11", "TRT12", "TRT13", "TRT14", "TRT15", "TRT16", "TRT17", "TRT18", "TRT19", "TRT20", "TRT21", "TRT22", "TRT23", "TRT24")

    ### Arquivos do DPJ ----
        
    outputFolder <- "output/Painel 2025/"
    path <- "output/"
    if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)      

    arq <- list.files(path, pattern = "csv")
    arq <- arq[substr(arq, 1, 1) %in% c("T", "S")]


    i=arq[1]
    print(arq)

    ref = 20260101

    # uf, nome_municipio
    nome_oj <- fread("//fluor02.cnj.jus.br/SEP/DPJ Pnud/Eixo04/painel_estatistica/BI/desenvolvimento/Orgaos_julgadores_R.csv")
    nome_oj <- nome_oj %>% select(c("id_orgao_julgador","orgao_julgador", "uf_oj", "municipio_oj"))
    nome_oj <- distinct(nome_oj)
    names(nome_oj)[names(nome_oj) == 'orgao_julgador'] <- 'nome'
    nome_oj$id_orgao_julgador[is.na(nome_oj$id_orgao_julgador)] <- -1
    local <- nome_oj[, c("uf_oj", "nome", "municipio_oj", "id_orgao_julgador")]

    for (i in arq) {

        RegistrarLOG(paste0("Processando dados do ", i))
        dge <- fread(paste0(path, i))
  
        dge <- dge %>%
            mutate(
                mes_cnm1 = substring(dt_recebimento,6,7),
                mes_senth = substring(dt_primeiro_julgamento_homologatorio,6,7),
                mes_sent = substring(dt_resolvido,6,7))
  
        dge <- dge %>% left_join(local, by=c("id_ultimo_oj"="id_orgao_julgador"))
        dge <- distinct(dge)
        
        meta1 <- Consolidado.Meta01(dge)
        
        if (any(dge$sigla_tribunal %in% STM)) {
            metas <- Consolidado.STM(dge)
            meta2 <- metas$meta2
            meta4 <- metas$meta4
            list_metas = list(meta1, meta2, meta4)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.STM(metas_compilado)
        }
        
        else if (any(dge$sigla_tribunal %in% JusticaMilitar)) {
            metas <- Consolidado.JusticaMilitar(dge)
            meta2 <- metas$meta2
            meta4 <- metas$meta4
            list_metas = list(meta1, meta2, meta4)
            metas_compilado <- list_metas %>% reduce(bind_rows)            
            Resumo.JusticaMilitar(metas_compilado)
        }
        
        else if (any(dge$sigla_tribunal %in% JusticaEleitoral)) {
            metas <- Consolidado.JusticaEleitoral(dge)
            meta2 <- metas$meta2
            meta4 <- metas$meta4
            list_metas = list(meta1, meta2, meta4)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.JusticaEleitoral(metas_compilado)
        } 
        
        else if (any(dge$sigla_tribunal %in% JusticaFederal)) {
            metas <- Consolidado.JusticaFederal(dge)
            meta2 <- metas$meta2
            meta3 <- metas$meta3
            meta4 <- metas$meta4
            meta6 <- metas$meta6
            meta7 <- metas$meta7
            meta10 <- metas$meta10
            list_metas = list(meta1, meta2, meta3, meta4, meta6, meta7, meta10)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.JusticaFederal(metas_compilado)
        } 
  
        else if (any(dge$sigla_tribunal %in% JusticaEstadual)) {

            metas <- Consolidado.JusticaEstadual(dge)
            meta2 <- metas$meta2
            meta3 <- metas$meta3
            meta4 <- metas$meta4
            meta6 <- metas$meta6
            meta7 <- metas$meta7
            meta8 <- metas$meta8
            meta10 <- metas$meta10
            list_metas = list(meta1, meta2, meta3, meta4, meta6, meta7, meta8, meta10)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.JusticaEstadual(metas_compilado)
        } 
        
        else if (any(dge$sigla_tribunal %in% JusticaTrabalho)) {
            metas <- Consolidado.JusticaTrabalho(dge)
            meta2 <- metas$meta2
            meta3 <- metas$meta3
            list_metas = list(meta1, meta2, meta3)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.JusticaTrabalho(metas_compilado)
            
        } 
        
        else if(unique(dge$sigla_tribunal) == "STJ"){
            metas <- Consolidado.STJ(dge)
            meta2 <- metas$meta2
            meta4 <- metas$meta4
            meta6 <- metas$meta6
            meta7 <- metas$meta7
            meta8 <- metas$meta8
            meta10 <- metas$meta10
            list_metas = list(meta1, meta2, meta4, meta6, meta7, meta8, meta10)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.STJ(metas_compilado)
            
        }
  
        else if(unique(dge$sigla_tribunal) == "TST"){
            metas <- Consolidado.TST(dge)
            meta2 <- metas$meta2
            list_metas = list(meta1, meta2)
            metas_compilado <- list_metas %>% reduce(bind_rows)
            Resumo.TST(metas_compilado)
            
        }
        

        Painel <- bind_rows(Painel, distinct(metas_compilado))
    
    }

    # Painel <- Painel %>% mutate(data_referencia = "DD/MM/AAAA") # Esta é a d
    
    save(Painel, file = paste0(outputFolder, "Painel_25.RData"))
    data.table::fwrite(Painel, file = paste0(outputFolder, "/Painel_25.csv"))
    unique(Painel$sigla_tribunal)

} # Final da função



