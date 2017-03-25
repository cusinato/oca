
library(googlesheets)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# csv <- 'respostas - 20170320_fechado.csv'
csv <- 'teste - respostas.csv'

# nome_planilha <- 'Cesta JK AgroSustentável 22 de Março (respostas)'
nome_planilha <- 'Cesta JK AgroSustentável 29 de Março (respostas)'

# nome_planilha <- 'OCA - Orgânicos do Cerrado Agroecológico'

gs_title(nome_planilha) %>%
  gs_download(to = csv, overwrite = T)

csv %>% read_csv() %>% select(ts = one_of("Indicação de data e hora"),
                              nome = one_of("NOME COMPLETO"),
                              celular = one_of("CELULAR"),
                              email = one_of("E-MAIL"),
                              endereco = one_of("ENDEREÇO COMPLETO"),
                              bairro = one_of("BAIRRO"),
                              cep = one_of("CEP"),
                              pagamento = one_of("FORMA DE PAGAMENTO"),
                              entrega = one_of("Preferência de Entrega"),
                              everything()) -> cru
# View(cru)

precos <- data_frame(tx = names(cru)[-c(1:9)]) %>%
  filter(!(tx %>% str_to_lower %>% str_detect('pagamento'))) %>% 
  separate(tx, c("produto_medida", "preco"), sep = " - R\\$ ", remove = F) %>% 
  mutate(preco = gsub(',', '.', preco) %>% as.numeric,
         produto = gsub('(.*)\\(.+\\)', '\\1', produto_medida),
         medida = gsub('.*\\((.+)\\).*', '\\1', produto_medida))
# View(precos)

tb <- cru %>% gather(key = item, value = resp,
                     -ts, -nome, -celular, -email, -endereco, -bairro, -cep, -pagamento, -entrega)# %>% 
  # unnest(item = strsplit(item, ', '))
# glimpse(tb)
# View(tb)

# tratamento diferenciado pra poupas de frutas
# abordagem: transformar indicadoras em quantidades

## arquivo de saida:
# vazio
# nome (bairro) telefone*
# forma de pagamento - entrega*
# endereco
# cabecalho
# produtos
# produtos
# produtos
# produtos
# quantidade total, valor total
# taxa de entrega
# vazio
lema <- function(pessoa = NULL, tb){
  if(is.null(pessoa)){
    tb %>%
      filter(!is.na(resp)) %>% 
      left_join(precos, by=c('item' = 'tx')) %>% 
      mutate(qt = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
                          resp %>% str_count(',') + 1.0,
                          if_else(resp %>% str_to_lower() %>% str_detect('mei[ao]'),
                                  0.5,
                                  gsub('\\D', '', resp) %>% as.numeric())),
             total = qt * preco,
             tx_total = gsub('\\.', ',', sprintf('R$ %.2f', qt * preco)),
             produto = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
                               sprintf('%s (%s)', produto, resp),
                               produto),
             tx_preco = gsub('\\.', ',', sprintf('R$ %.2f', preco)))
  } else {
    tb %>%
      filter(!is.na(resp), nome == pessoa) %>% 
      left_join(precos, by=c('item' = 'tx')) %>% 
      mutate(qt = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
                          resp %>% str_count(',') + 1.0,
                          if_else(resp %>% str_to_lower() %>% str_detect('mei[ao]'),
                                  0.5,
                                  gsub('\\D', '', resp) %>% as.numeric())),
             total = qt * preco,
             tx_total = gsub('\\.', ',', sprintf('R$ %.2f', qt * preco)),
             produto = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
                               sprintf('%s (%s)', produto, resp),
                               produto),
             tx_preco = gsub('\\.', ',', sprintf('R$ %.2f', preco)))
  }
}
trata_pessoa <- function(pessoa, tb){
  # pessoa = 'suely borba'
  tb2 <- lema(pessoa, tb)
  tb_ttl <- summarise(tb2, qt = sum(qt), ttl = sum(total))
  mat <- tb2 %>% select(produto, medida, tx_preco, qt, tx_total) %>% as.matrix()
  cad <- matrix('', 5, ncol(mat))
  cad[nrow(cad), ] <- c('Produto', 'Unidade de medida', 'Valor unitário', 'Quantidade', 'Valor total')
  cad[2:4, 1] <- c(
    sprintf("%s (%s) %s", tb2$nome[1], tb2$bairro[1], tb2$celular[1]), 
    tb2$pagamento[1],
    sprintf("%s -%s", tb2$endereco[1], gsub("entre", "", tb2$entrega[1], ignore.case = T)))
  ttl <- matrix('', 3, ncol(mat))
  ttl[1, 4:5] <- c(tb_ttl$qt, paste('R$', tb_ttl$ttl))
  ttl[2, 3] <- 'Taxa de entrega:'
  saida <- rbind(cad, mat, ttl)
  colnames(saida) <- NULL
  saida
}
# trata_pessoa('Kelly Gomes', tb)

# geral <- tb %>%
#   group_by(ts, nome, celular, email, endereco, bairro, cep, pagamento) %>% 
#   do(trata_pessoa(.))

# geral <- tb %>%
#   group_by(ts, nome, celular, email, endereco, bairro, cep, pagamento) %>% 
#   do(filter(., !is.na(resp)) %>% 
#        left_join(precos, by=c('item' = 'tx')) %>% 
#        mutate(qt = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
#                            resp %>% str_count(',') + 1.0,
#                            if_else(resp %>% str_to_lower() %>% str_detect('mei[ao]'),
#                                    0.5,
#                                    gsub('\\D', '', resp) %>% as.numeric())
#                            ),
#               total = qt * preco,
#               produto = if_else(produto %>% str_to_lower() %>% str_detect('polpa de fruta'),
#                                 sprintf('%s (%s)', produto, resp),
#                                 produto)) %>% 
#        select(produto, medida, preco, qt, total))
# View(geral)

# View(ttl_cli)

ls_cli <- (tb %>% select(nome) %>% distinct)[[1]]

arq_saida <- gsub('respostas', 'pedidos', csv)
pedidos <- do.call(rbind, lapply(ls_cli, trata_pessoa, tb = tb)) %>% as_data_frame()
pedidos %>% write_csv(arq_saida, col_names = F)
arq_saida %>% gs_upload(sheet_title = gsub('\\.csv$', '', arq_saida))

arq_saida <- gsub('respostas', 'produtos', csv)
ttl_prd <- lema(tb = tb) %>%
  group_by(produto, medida, tx_preco) %>%
  summarise(qt_ttl = gsub('\\.', ',', sum(qt)),
            vl_ttl = gsub('\\.', ',', sprintf('R$ %.2f', sum(total)))) %>% 
  rename(Produto = produto,
         Medida = medida,
         'Valor unitário' = tx_preco,
         'Quantidade total' = qt_ttl,
         'Valor total' = vl_ttl)
# View(ttl_prd)
write_csv(ttl_prd, arq_saida)
arq_saida %>% gs_upload(sheet_title = gsub('\\.csv$', '', arq_saida))

arq_saida <- gsub('respostas', 'clientes', csv)
ttl_cli <- lema(tb = tb) %>%
  group_by(nome) %>%
  summarise(qt_ttl = gsub('\\.', ',', sum(qt)),
            vl_ttl = gsub('\\.', ',', sprintf('R$ %.2f', sum(total)))) %>% 
  rename(Cliente = nome,
         'Quantidade total' = qt_ttl,
         'Valor total' = vl_ttl)
# View(ttl_cli)
write_csv(ttl_cli, arq_saida)
arq_saida %>% gs_upload(sheet_title = gsub('\\.csv$', '', arq_saida))
