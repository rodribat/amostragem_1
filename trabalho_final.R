
library(readxl)
library(dplyr)

################################################################################
# LEITURA DO ARQUIVO
################################################################################

df <- read_xlsx('dados-ava-03.xlsx')


################################################################################
# SELEÇÃO DE VARIÁVEIS
################################################################################

# Z001	Sexo
# C008	Idade da base do IBGE
# Z003 	Cor ou raça 
# Z004 	Peso - Final (em kg)
# Z005 	Altura - Final (em cm)
# C010	_______ vive com cônjuge ou companheiro(a)?
# C011	Qual é o estado civil de ___?
# W00303	Circunferência da cintura - Final (em cm)
# regiao	Região

df <- df[,c('Z001', 'C008', 'Z003', 'Z004', 'Z005', 'C010', 'C011', 'W00303', 'REGIAO')]
names(df) <- c('sexo', 'idade', 'raca', 'peso', 'altura', 'conjuge', 'estado_civil', 'cintura', 'regiao')


################################################################################
# TRATAMENTO DAS VARIÁVEIS
################################################################################

# sexo
sexo_dict <- c('1'='Masculino', '2'='Feminino')
df$sexo <- sexo_dict[df$sexo]

# raca
raca_dict <- c('1'='Branca',
          '2'='Preta',
          '3'='Amarela',
          '4'='Parda',
          '5'='Indígena',
          '9'='Ignorado')
df$raca <- raca_dict[df$raca]

# conjuge
conjuge_dict <- c('1'='Sim', '2'='Não')
df$conjuge <- conjuge_dict[df$conjuge]

# estado civil
estado_civil_dict <- c('1'='Casado',
                       '2'='Separado',
                       '3'='Divorciado',
                       '4'='Viúvo',
                       '5'='Solteiro',
                       '.'='Não aplicável')
df$estado_civil <- estado_civil_dict[df$estado_civil]

# regiao
regiao_dict <- c('1'='Norte',
                 '2'='Nordeste',
                 '3'='Sudeste',
                 '4'='Sul',
                 '5'='Centro-Oeste')
df$regiao <- regiao_dict[df$regiao]

# peso
df[df$peso==999,] <- NA

# altura
df[df$altura==999,] <- NA

# imc
df$imc <- df$peso / (df$altura/100)^2


################################################################################
# TRATAMENTO DE NA's
################################################################################

# remoção de registros onde não foi possível calcular o IMC
df <- df[!is.na(df$imc),]


################################################################################
#  DISTRIBUIÇÃO DO IMC
################################################################################

hist(df$imc)


################################################################################
#  OUTRAS ANÁLISES
################################################################################

# Teste com base em conjuge
df_conj <- df %>% 
  group_by(conjuge) %>% 
  summarise(n = n(),
            imc_medio = mean(imc),
            imc_dp = sd(imc)) %>% 
  arrange(conjuge) %>% 
  ungroup()

t.test(df[df$conjuge=='Sim','imc'], df[df$conjuge=='Não','imc'])


# Teste com base em estado civil
df_estado_civil <- df %>% 
  group_by(estado_civil) %>% 
  summarise(n = n(),
            imc_medio = mean(imc),
            imc_dp = sd(imc)) %>% 
  arrange(estado_civil) %>% 
  ungroup()

modelo_anova <- aov(imc ~ estado_civil, data = df)
summary(modelo_anova)
