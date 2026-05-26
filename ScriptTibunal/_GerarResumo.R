

#

Resumo.STM <- function(dge) {

    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgm2_a,na.rm = TRUE)/(sum(distm2_a,na.rm = TRUE)-sum(suspm2_a,na.rm = TRUE)))*(1000/9.5),
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgm2_b,na.rm = TRUE)/(sum(distm2_b,na.rm = TRUE)-sum(suspm2_b,na.rm = TRUE)))*(1000/9.9),
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgm2_ant,na.rm = TRUE)/(sum(distm2_ant,na.rm = TRUE)-sum(suspm2_ant,na.rm = TRUE)))*(1000/10),
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgm4_a,na.rm = TRUE)/(sum(distm4_a,na.rm = TRUE)-sum(suspm4_a,na.rm = TRUE)))*(1000/9.5),
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgm4_b,na.rm = TRUE)/(sum(distm4_b,na.rm = TRUE)-sum(suspm4_b,na.rm = TRUE)))*(1000/9.9),
            .groups = "drop"
        )

    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}











Resumo.JusticaMilitar <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgm2_a,na.rm = TRUE)/(sum(distm2_a,na.rm = TRUE)-sum(suspm2_a,na.rm = TRUE)))*(1000/9),
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (sum(julgm2_b,na.rm = TRUE)/(sum(distm2_b,na.rm = TRUE)-sum(suspm2_b,na.rm = TRUE)))*(1000/9.5),
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (sum(julgm2_ant,na.rm = TRUE)/(sum(distm2_ant,na.rm = TRUE)-sum(suspm2_ant,na.rm = TRUE)))*(1000/10),
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgm4_a,na.rm = TRUE)/(sum(distm4_a,na.rm = TRUE)-sum(suspm4_a,na.rm = TRUE)))*(1000/9.5),
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgm4_b,na.rm = TRUE)/(sum(distm4_b,na.rm = TRUE)-sum(suspm4_b,na.rm = TRUE)))*(1000/9.5),
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}



Resumo.JusticaEleitoral <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (sum(julgm2_a,na.rm = TRUE)/(sum(distm2_a,na.rm = TRUE)-sum(suspm2_a,na.rm = TRUE)))*(1000/7),
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant /(distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (sum(julgm4_a,na.rm = TRUE)/(sum(distm4_a,na.rm = TRUE)-sum(suspm4_a,na.rm = TRUE)))*(1000/9),
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (sum(julgm4_b,na.rm = TRUE)/(sum(distm4_b,na.rm = TRUE)-sum(suspm4_b,na.rm = TRUE)))*(1000/5),
            .groups = "drop"
            
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}





Resumo.JusticaFederal <- function(dge) {
    
    if (unique(dge$sigla_tribunal) %in% c("TRF1", "TRF6"))fator <- 1000/2.5
    else fator <- 1000/3.5        # TRF2, 3, 4, 5
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a /( distm2_a - suspm2_a))*(1000/8.5),
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (julgm2_b / (distm2_b - suspm2_b))*(1000/10),
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE), 
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant / (distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            
            IC2024 = (sum(quant_conc24,na.rm = TRUE))/sum(quant_sent24,na.rm = TRUE),
            IC2025 = (sum(quant_conc24,na.rm = TRUE))/sum(quant_sent25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC2024+0.005),(IC2025/(IC2024+0.005))*100,if_else(IC2025>="0.08",100,(IC2025/(IC2024+0.005))*100)),
            

            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a / (distm4_a - suspm4_a))*(1000/7),
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (julgm4_b / (distm4_b - suspm4_b))*(100),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a / (distm6_a - suspm6_a))*fator,
            
            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a))*fator,
            
            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b/(distm7_b-suspm7_b))*fator,
            
            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a/(distm10_a - suspm10_a))*(1000/10),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}






Resumo.JusticaEstadual <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a / (distm2_a - suspm2_a))*(1000/8),
            
            distm2_b = sum(distm2_b,na.rm = TRUE),
            julgm2_b = sum(julgm2_b,na.rm = TRUE),
            suspm2_b = sum(suspm2_b,na.rm = TRUE),
            cumprimento_meta2b = (julgm2_b / (distm2_b - suspm2_b))*(1000/9),
            
            distm2_c = sum(distm2_c,na.rm = TRUE),
            julgm2_c = sum(julgm2_c,na.rm = TRUE),
            suspm2_c = sum(suspm2_c,na.rm = TRUE),
            cumprimento_meta2c = (julgm2_c / (distm2_c - suspm2_c))*(1000/9.5),
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE), 
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = (julgm2_ant /(distm2_ant - suspm2_ant - desom2_ant))*(1000/10),
            
            IC2024 = (sum(quant_conc24,na.rm = TRUE))/sum(quant_sent24,na.rm = TRUE),
            IC2025 = (sum(quant_conc25,na.rm = TRUE))/sum(quant_sent25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC2024+0.01),(IC2025/(IC2024+0.01))*100,if_else(IC2025>="0.17",100,(IC2025/(IC2024+0.01))*100)),
            
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a/(distm4_a - suspm4_a))*(1000/6.5),
            
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (julgm4_b /(distm4_b -suspm4_b))*(1000/10),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a /(distm6_a-suspm6_a))*(1000/5),

            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a/(distm7_a - suspm7_a))*(1000/5),
            
            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b /(distm7_b - suspm7_b))*(1000/5),

            distm8_a = sum(distm8_a,na.rm = TRUE),
            julgm8_a = sum(julgm8_a,na.rm = TRUE),
            suspm8_a = sum(suspm8_a,na.rm = TRUE),
            cumprimento_meta8a = (julgm8_a /(distm8_a - suspm8_a))*(1000/9),
            
            distm8_b = sum(distm8_b,na.rm = TRUE),
            julgm8_b = sum(julgm8_b,na.rm = TRUE),
            suspm8_b = sum(suspm8_b,na.rm = TRUE),
            cumprimento_meta8b = (julgm8_b /(distm8_b-suspm8_b))*(1000/7.5),

            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a /(distm10_a - suspm10_a))*(1000/9),
            
            distm10_b = sum(distm10_b,na.rm = TRUE),
            julgm10_b = sum(julgm10_b,na.rm = TRUE),
            suspm10_b = sum(suspm10_b,na.rm = TRUE),
            cumprimento_meta10b = (julgm10_b / (distm10_b - suspm10_b))*(1000/10),

            .groups = "drop"
        )
    print(Resumo)
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}

Resumo.JusticaTrabalho <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = julgm2_a/(distm2_a - suspm2_a)*(1000/9.4),
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE),
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = julgm2_ant/(distm2_ant-desom2_ant-suspm2_ant)*(1000/10),


            IC22_23 = (sum(quant_conc22_23,na.rm = TRUE))/sum(quant_sent22_23,na.rm = TRUE),
            IC2025 = (sum(quant_conc25,na.rm = TRUE))/sum(quant_sent25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC2025 > (IC22_23+0.005),(IC2025/(IC22_23+0.005))*100,if_else(IC2025>="0.38",100,(IC2025/(IC22_23+0.005))*100)),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}


Resumo.STJ <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            
            distm2_a = sum(distm2_a,na.rm = TRUE),
            julgm2_a = sum(julgm2_a,na.rm = TRUE),
            suspm2_a = sum(suspm2_a,na.rm = TRUE),
            cumprimento_meta2a = (julgm2_a  / (distm2_a - suspm2_a))*(1000/10), #Verificar questão dos dessobrestados
        
            distm4_a = sum(distm4_a,na.rm = TRUE),
            julgm4_a = sum(julgm4_a,na.rm = TRUE),
            suspm4_a = sum(suspm4_a,na.rm = TRUE),
            cumprimento_meta4a = (julgm4_a / (distm4_a - suspm4_a))*(1000/9),
            distm4_b = sum(distm4_b,na.rm = TRUE),
            julgm4_b = sum(julgm4_b,na.rm = TRUE),
            suspm4_b = sum(suspm4_b,na.rm = TRUE),
            cumprimento_meta4b = (julgm4_b / (distm4_b - suspm4_b))*(1000/10),
            
            distm6_a = sum(distm6_a,na.rm = TRUE),
            julgm6_a = sum(julgm6_a,na.rm = TRUE),
            suspm6_a = sum(suspm6_a,na.rm = TRUE),
            cumprimento_meta6a = (julgm6_a / (distm6_a - suspm6_a))*(1000/7.5),
            
            distm7_a = sum(distm7_a,na.rm = TRUE),
            julgm7_a = sum(julgm7_a,na.rm = TRUE),
            suspm7_a = sum(suspm7_a,na.rm = TRUE),
            cumprimento_meta7a = (julgm7_a / (distm7_a - suspm7_a))*(1000/7.5),
            distm7_b = sum(distm7_b,na.rm = TRUE),
            julgm7_b = sum(julgm7_b,na.rm = TRUE),
            suspm7_b = sum(suspm7_b,na.rm = TRUE),
            cumprimento_meta7b = (julgm7_b / (distm7_b - suspm7_b))*(1000/7.5),
            
            distm8_a = sum(distm8_a,na.rm = TRUE),
            julgm8_a = sum(julgm8_a,na.rm = TRUE),
            suspm8_a = sum(suspm8_a,na.rm = TRUE),
            cumprimento_meta8a = (julgm8_a / (distm8_a - suspm8_a))*(1000/10),
            distm8_b = sum(distm8_b,na.rm = TRUE),
            julgm8_b = sum(julgm8_b,na.rm = TRUE),
            suspm8_b = sum(suspm8_b,na.rm = TRUE),
            cumprimento_meta8b = (julgm8_b / (distm8_b - suspm8_b))*(1000/10),
            
            distm10_a = sum(distm10_a,na.rm = TRUE),
            julgm10_a = sum(julgm10_a,na.rm = TRUE),
            suspm10_a = sum(suspm10_a,na.rm = TRUE),
            cumprimento_meta10a = (julgm10_a /(distm10_a - suspm10_a))*(1000/10),
            
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}

Resumo.TST <- function(dge) {
    
    Resumo <- dge %>%
        group_by(sigla_tribunal) %>%
        summarise (
            sigla_tribunal = min(sigla_tribunal),
            
            casos_novos_2025 = sum(casos_novos_2025,na.rm = TRUE),
            julgados_2025 = sum(julgados_2025,na.rm = TRUE),
            prim_sent2025 = sum(prim_sent2025, na.rm = TRUE),
            suspensos_2025 = sum(suspensos_2025,na.rm = TRUE),
            dessobrestados_2025 = sum(dessobrestados_2025,na.rm = TRUE),
            cumprimento_meta1 = julgados_2025/(casos_novos_2025 + dessobrestados_2025 - suspensos_2025)*100,
            
            distm2_ant = sum(distm2_ant,na.rm = TRUE),
            julgm2_ant = sum(julgm2_ant,na.rm = TRUE),
            suspm2_ant = sum(suspm2_ant,na.rm = TRUE), 
            desom2_ant = sum(desom2_ant,na.rm = TRUE),
            cumprimento_meta2ant = ( julgm2_ant /(distm2_ant - desom2_ant - suspm2_ant))*(1000/10),
            .groups = "drop"
        )
    
    GerarResumo(Resumo, unique(dge$sigla_tribunal))
    
}
