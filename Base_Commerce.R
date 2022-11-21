[#### Dicionary dataset
### Relations
## Order Reviews (order_id) # Reviews Orders
## Order Status (order_id, customer_id) # Status Order
## Order Payment (order_id) # Payment method
## Order Items (order_id, products_id, seller_id) # Products Sells
## Customer (customer_id, zip_code_prefix) # Customers
## Geo Customer (zip_code_prefix) # Customers Locations
## Sellers (zip_code_prefix, seller_id) # Delivery City
## Products (products_id) # All Products

#### Paramaters
customers_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_customers_dataset.csv') # nolint
geo_customers_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_geolocation_dataset.csv') # nolint
orders_items_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_order_items_dataset.csv') # nolint
order_payments_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_order_payments_dataset.csv') # nolint
order_review_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_order_reviews_dataset.csv') # nolint
orders_status_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_orders_dataset.csv') # nolint
products_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_products_dataset.csv') # nolint'
sellers_path <- ('D:/green_lake/Projetos/e-commerce/data/olist_sellers_dataset.csv') # nolint


#### Functions
estate_brasil <- function(df) {
    df$customer_state[df$customer_state == "AC"] <- "acre"
    df$customer_state[df$customer_state == "AL"] <- "alagoas"
    df$customer_state[df$customer_state == "AP"] <- "amapa"
    df$customer_state[df$customer_state == "AM"] <- "amazonas"
    df$customer_state[df$customer_state == "BA"] <- "bahia"
    df$customer_state[df$customer_state == "CE"] <- "ceara"
    df$customer_state[df$customer_state == "ES"] <- "espirito santo"
    df$customer_state[df$customer_state == "DF"] <- "distrito federal"
    df$customer_state[df$customer_state == "GO"] <- "goias"
    df$customer_state[df$customer_state == "MA"] <- "maranhao"
    df$customer_state[df$customer_state == "MT"] <- "mato grosso"
    df$customer_state[df$customer_state == "MS"] <- "mato grosso do sul"
    df$customer_state[df$customer_state == "MG"] <- "minas gerais"
    df$customer_state[df$customer_state == "PA"] <- "para"
    df$customer_state[df$customer_state == "PB"] <- "paraiba"
    df$customer_state[df$customer_state == "PE"] <- "pernambuco"
    df$customer_state[df$customer_state == "PR"] <- "parana"
    df$customer_state[df$customer_state == "PI"] <- "piaui"
    df$customer_state[df$customer_state == "RJ"] <- "rio de janeiro"
    df$customer_state[df$customer_state == "RS"] <- "rio grande do sul"
    df$customer_state[df$customer_state == "RO"] <- "rondonia"
    df$customer_state[df$customer_state == "RR"] <- "roraima"
    df$customer_state[df$customer_state == "SC"] <- "santa catarina"
    df$customer_state[df$customer_state == "SE"] <- "sergipe"
    df$customer_state[df$customer_state == "SP"] <- "sao paulo"
    df$customer_state[df$customer_state == "TO"] <- "tocantins"
    return(df)
}

estate_brasil_v2 <- function(df) {
    df$seller_state[df$seller_state == "AC"] <- "acre"
    df$seller_state[df$seller_state == "AL"] <- "alagoas"
    df$seller_state[df$seller_state == "AP"] <- "amapa"
    df$seller_state[df$seller_state == "AM"] <- "amazonas"
    df$seller_state[df$seller_state == "BA"] <- "bahia"
    df$seller_state[df$seller_state == "CE"] <- "ceara"
    df$seller_state[df$seller_state == "ES"] <- "espirito santo"
    df$seller_state[df$seller_state == "DF"] <- "distrito federal"
    df$seller_state[df$seller_state == "GO"] <- "goias"
    df$seller_state[df$seller_state == "MA"] <- "maranhao"
    df$seller_state[df$seller_state == "MT"] <- "mato grosso"
    df$seller_state[df$seller_state == "MS"] <- "mato grosso do sul"
    df$seller_state[df$seller_state == "MG"] <- "minas gerais"
    df$seller_state[df$seller_state == "PA"] <- "para"
    df$seller_state[df$seller_state == "PB"] <- "paraiba"
    df$seller_state[df$seller_state == "PE"] <- "pernambuco"
    df$seller_state[df$seller_state == "PR"] <- "parana"
    df$seller_state[df$seller_state == "PI"] <- "piaui"
    df$seller_state[df$seller_state == "RJ"] <- "rio de janeiro"
    df$seller_state[df$seller_state == "RS"] <- "rio grande do sul"
    df$seller_state[df$seller_state == "RO"] <- "rondonia"
    df$seller_state[df$seller_state == "RR"] <- "roraima"
    df$seller_state[df$seller_state == "SC"] <- "santa catarina"
    df$seller_state[df$seller_state == "SE"] <- "sergipe"
    df$seller_state[df$seller_state == "SP"] <- "sao paulo"
    df$seller_state[df$seller_state == "TO"] <- "tocantins"
    return(df)
}


estate_brasil_mes_compra <- function(df){
    df$mes_compra[df$mes_compra == "January"] <- "janeiro"
    df$mes_compra[df$mes_compra == "February"] <- "fevereiro"
    df$mes_compra[df$mes_compra == "March"] <- "marco"
    df$mes_compra[df$mes_compra == "April"] <- "abril"
    df$mes_compra[df$mes_compra == "May"] <- "maio"
    df$mes_compra[df$mes_compra == "June"] <- "junho"
    df$mes_compra[df$mes_compra == "July"] <- "julho"
    df$mes_compra[df$mes_compra == "August"] <- "agosto"
    df$mes_compra[df$mes_compra == "September"] <- "setembro"
    df$mes_compra[df$mes_compra == "October"] <- "outubro"
    df$mes_compra[df$mes_compra == "November"] <- "novembro"
    df$mes_compra[df$mes_compra == "December"] <- "dezembro"

    return(df)
}

estate_brasil_mes_aprovacao <- function(df){
    df$mes_aprovacao[df$mes_aprovacao == "January"] <- "janeiro"
    df$mes_aprovacao[df$mes_aprovacao == "February"] <- "fevereiro"
    df$mes_aprovacao[df$mes_aprovacao == "March"] <- "marco"
    df$mes_aprovacao[df$mes_aprovacao == "April"] <- "abril"
    df$mes_aprovacao[df$mes_aprovacao == "May"] <- "maio"
    df$mes_aprovacao[df$mes_aprovacao == "June"] <- "junho"
    df$mes_aprovacao[df$mes_aprovacao == "July"] <- "julho"
    df$mes_aprovacao[df$mes_aprovacao == "August"] <- "agosto"
    df$mes_aprovacao[df$mes_aprovacao == "September"] <- "setembro"
    df$mes_aprovacao[df$mes_aprovacao == "October"] <- "outubro"
    df$mes_aprovacao[df$mes_aprovacao == "November"] <- "novembro"
    df$mes_aprovacao[df$mes_aprovacao == "December"] <- "dezembro"

    return(df)
}

estate_brasil_mes_envio <- function(df){
    df$mes_envio[df$mes_envio == "January"] <- "janeiro"
    df$mes_envio[df$mes_envio == "February"] <- "fevereiro"
    df$mes_envio[df$mes_envio == "March"] <- "marco"
    df$mes_envio[df$mes_envio == "April"] <- "abril"
    df$mes_envio[df$mes_envio == "May"] <- "maio"
    df$mes_envio[df$mes_envio == "June"] <- "junho"
    df$mes_envio[df$mes_envio == "July"] <- "julho"
    df$mes_envio[df$mes_envio == "August"] <- "agosto"
    df$mes_envio[df$mes_envio == "September"] <- "setembro"
    df$mes_envio[df$mes_envio == "October"] <- "outubro"
    df$mes_envio[df$mes_envio == "November"] <- "novembro"
    df$mes_envio[df$mes_envio == "December"] <- "dezembro"

    return(df)
}

estate_brasil_mes_estimado <- function(df){
    df$mes_estimado_entrega[df$mes_estimado_entrega == "January"] <- "janeiro"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "February"] <- "fevereiro" # nolint
    df$mes_estimado_entrega[df$mes_estimado_entrega == "March"] <- "marco"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "April"] <- "abril"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "May"] <- "maio"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "June"] <- "junho"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "July"] <- "julho"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "August"] <- "agosto"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "September"] <- "setembro" # nolint
    df$mes_estimado_entrega[df$mes_estimado_entrega == "October"] <- "outubro"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "November"] <- "novembro"
    df$mes_estimado_entrega[df$mes_estimado_entrega == "December"] <- "dezembro"

    return(df)
}

estate_brasil_mes_entrega <- function(df){
    df$mes_entrega[df$mes_entrega == "January"] <- "janeiro"
    df$mes_entrega[df$mes_entrega == "February"] <- "fevereiro"
    df$mes_entrega[df$mes_entrega == "March"] <- "marco"
    df$mes_entrega[df$mes_entrega == "April"] <- "abril"
    df$mes_entrega[df$mes_entrega == "May"] <- "maio"
    df$mes_entrega[df$mes_entrega == "June"] <- "junho"
    df$mes_entrega[df$mes_entrega == "July"] <- "julho"
    df$mes_entrega[df$mes_entrega == "August"] <- "agosto"
    df$mes_entrega[df$mes_entrega == "September"] <- "setembro"
    df$mes_entrega[df$mes_entrega == "October"] <- "outubro"
    df$mes_entrega[df$mes_entrega == "November"] <- "novembro"
    df$mes_entrega[df$mes_entrega == "December"] <- "dezembro"

    return(df)
}
source("D:/green_lake/codigos/automatizações/RemoveAcentos.R")

#### Load Base
customers <- read.csv2(customers_path, sep  = ",") # load base customers
geo_customers <- read.csv2(geo_customers_path, sep = ",") # load base geo location  # nolint
orders_items <- read.csv2(orders_items_path, sep = ',') # load base orders # nolint
order_payments <- read.csv2(order_payments_path, sep = ',') # load base orders payments # nolint
order_review <- read.csv2(order_review_path, sep = ',') # load base review # nolint
orders_status <- read.csv2(orders_status_path, sep = ',') # load base order status # nolint
products <- read.csv2(products_path, sep = ',') # load base products # nolint
sellers <- read.csv2(sellers_path, sep = ',') # load base sellers total # nolint
    rm(customers_path,geo_customers_path,
       order_payments_path,order_review_path,
       orders_items_path,orders_status_path,
       products_path,sellers_path) # remove files paths

### Load Geo location Brazil
region <- geobr::read_state(
    year = 2017,
    simplified = FALSE)
        region <- region[c(3,5)] # reformart columns
            region$name_region <- tolower(region$name_region) # tolower column
            region$name_state <- tolower(region$name_state) # tolower column
                region$name_state <- RemoveAcentos(region$name_state) # Remove accents #nolint
                    region$geom <- NULL # Remove column
region <- unique(region) # unique regions

#### Format Bases
customers <- estate_brasil(customers) # Format estate Brazil

customers <- dplyr::left_join(customers , region , by = c("customer_state" = "name_state")) # Add region in customers # nolint
    names(customers)[6] <- "customer_region" # Rename Column


products <- products[c(1,2,5)] # reformart columns
orders_items$order_item_id <- "0"
orders_items <- unique(orders_items)
orders_items <- dplyr::left_join(orders_items , products , by = c("product_id")) # Bring products names end number photos # nolint
    orders_items$products_recurrent <- duplicated(orders_items$product_id)
        orders_items$products_recurrent[orders_items$products_recurrent == "FALSE"] <- "unico" # informs if the product is recurring # nolint
        orders_items$products_recurrent[orders_items$products_recurrent == "TRUE"] <- "recorrente" # informs if the product is recurring # nolint

orders_items$product_category_name <- stringr::str_replace_all(orders_items$product_category_name, "_", " ") # Remove underline # nolint
orders_items$shipping_limit_date <- stringr::str_sub(orders_items$shipping_limit_date, start = 1 , end = 10) # Add Data Column # nolint


orders_items <- dplyr::left_join(orders_items , orders_status , by = c("order_id")) # Add status delivery # nolint
orders_items <- dplyr::left_join(orders_items , sellers, by = c("seller_id")) # Add State and Region # nolint
    orders_items <- estate_brasil_v2(orders_items) # Rename State
        names(region)[2] <- "seller_name_region"
        orders_items <- dplyr::left_join(orders_items , region, by = c("seller_state" = "name_state")) # Add region sellers # nolint

orders_items$order_purchase_timestamp <- stringr::str_sub(orders_items$order_purchase_timestamp , start = 1 , end = 10) # Add Data Column # nolint
    orders_items$order_purchase_timestamp <- as.Date(orders_items$order_purchase_timestamp, format = "%Y-%m-%d") # Change to data format # nolint
        orders_items$month_purcharse <- months(orders_items$order_purchase_timestamp) # Add Month # nolint

orders_items <- dplyr::left_join(orders_items , customers , by = c("customer_id")) # Add informs geo costumers # nolint    

orders_items <- dplyr::left_join(orders_items , order_payments , by = c("order_id")) # Add method form # nolint

order_review$review_creation_date <- stringr::str_sub(order_review$review_creation_date, start = 1, end = 10) # Add Data # nolint
order_review$review_answer_timestamp <- stringr::str_sub(order_review$review_answer_timestamp, start = 1, end = 10) # Add Data # nolint

orders_items <- dplyr::left_join(orders_items , order_review , by = c("order_id")) # Add Review # nolint

orders_items$order_approved_at <- stringr::str_sub(orders_items$order_approved_at , start = 1 , end = 10) # Add Data Column # nolint
    orders_items$month_approved <- as.Date(orders_items$order_approved_at, format = "%Y-%m-%d") # Change to data format # nolint

orders_items$order_delivered_carrier_date <- stringr::str_sub(orders_items$order_delivered_carrier_date , start = 1 , end = 10) # Add Data Column # nolint
    orders_items$order_delivered_carrier_date <- as.Date(orders_items$order_delivered_carrier_date, format = "%Y-%m-%d") # Change to data format # nolint


orders_items$order_delivered_customer_date <- stringr::str_sub(orders_items$order_delivered_customer_date , start = 1 , end = 10) # Add Data Column # nolint
    orders_items$order_delivered_customer_date <- as.Date(orders_items$order_delivered_customer_date, format = "%Y-%m-%d") # Change to data format # nolint

orders_items$order_estimated_delivery_date <- stringr::str_sub(orders_items$order_estimated_delivery_date , start = 1 , end = 10) # Add Data Column # nolint
    orders_items$order_estimated_delivery_date <- as.Date(orders_items$order_estimated_delivery_date, format = "%Y-%m-%d") # Change to data format # nolint

result <- orders_items[c(1,3,4,11,8,9,10,12,6,7,31,13:15,17,16,5,28:30,32:37,18:22,24:27)] # nolint
    rm(customers,geo_customers,order_payments,order_review,orders_items,products,region,sellers,orders_status) # remove dfs #nolint
        gc() # clear memory # nolint

names(result) <- c("ordem_id",
                   "produto_id",
                   "venda_id",
                   "cliente_id",
                   "produto_categoria",
                   "quantidade_fotos_produtos",
                   "produto_recorrente",
                   "ordem_status",
                   "valor_produto",
                   "valor_frete",
                   "valor_pago",
                   "data_compra",
                   "data_aprovacao",
                   "data_envio",
                   "data_estimada_entrega",
                   "data_entrega",
                   "data_limite",
                   "quantidade_pagamento",
                   "tipo_pagamento",
                   "parcelas",
                   "feedback_id",
                   "feedback_score",
                   "feedback_titulo",
                   "feedback_descricao",
                   "data_feedback",
                   "data_feedback_resposta",
                   "codigo_prefixo_entrega",
                   "entrega_cidade",
                   "entrega_estado",
                   "entrega_regiao",
                   "mes_compra",
                   "codigo_prefixo_cliente",
                   "cliente_cidade",
                   "cliente_estado",
                   "cliente_regiao"
                   )

result$ordem_status[result$ordem_status == "delivered"] <- "entregue"
result$ordem_status[result$ordem_status == "shipped"] <- "encaminhado"
result$ordem_status[result$ordem_status == "canceled"] <- "cancelado"
result$ordem_status[result$ordem_status == "invoiced"] <- "faturado"
result$ordem_status[result$ordem_status == "processing"] <- "processando"
result$ordem_status[result$ordem_status == "approved"] <- "aprovado"
result$ordem_status[result$ordem_status == "unavailable"] <- "indisponivel"

result <- estate_brasil_mes_compra(result)

result$data_aprovacao <- as.Date(result$data_aprovacao, format = "%Y-%m-%d")
result$mes_aprovacao <- months(result$data_aprovacao)
result$mes_envio <- months(result$data_envio)
result$mes_estimado_entrega <- months(result$data_estimada_entrega)
result$mes_entrega <- months(result$data_entrega)

result$tempo_compra_aprovacao <- difftime(result$data_aprovacao , result$data_compra , tz = "UTC" , units = "days") # nolint
    result$tempo_compra_aprovacao <- as.character(result$tempo_compra_aprovacao)

result$tempo_aprovacao_envio <- difftime(result$data_envio , result$data_aprovacao , tz = "UTC", units = "days") # nolint
    result$tempo_aprovacao_envio <- as.character(result$tempo_aprovacao_envio)

result$tempo_envio_entrega <- difftime(result$data_entrega , result$data_envio , tz = "UTC", units = "days") # nolint
    result$tempo_envio_entrega <- as.character(result$tempo_envio_entrega) # nolint

result$tempo_total_venda <- difftime(result$data_entrega , result$data_compra , tz = "UTC" , units = "days") # nolint
    result$tempo_total_venda <- as.character(result$tempo_total_venda)

result$data_estimada_entrega <- as.Date(result$data_estimada_entrega, format = "%Y-%m-%d")
result$data_limite <- as.Date(result$data_limite, format = "%Y-%m-%d")
    result$tempo_data_limite <- difftime(result$data_estimada_entrega , result$data_entrega , tz = "UTC" , units = "days") # nolint
        result$tempo_data_limite <- as.character(result$tempo_data_limite)

result$periodo_compra_aprovacao <- ""
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "0"] <- "imediato" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "1"] <- "1 a 5 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "2"] <- "1 a 5 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "3"] <- "1 a 5 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "4"] <- "1 a 5 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "5"] <- "1 a 5 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "6"] <- "5 a 10 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "7"] <- "5 a 10 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "8"] <- "5 a 10 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "9"] <- "5 a 10 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "10"] <- "5 a 10 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "11"] <- "10 a 15 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "12"] <- "10 a 15 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "13"] <- "10 a 15 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "14"] <- "10 a 15 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "15"] <- "10 a 15 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "16"] <- "15 a 20 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "17"] <- "15 a 20 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "18"] <- "15 a 20 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "19"] <- "15 a 20 dias" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "20"] <- "15 a 20 dias" # nolint

    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-1"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-2"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-3"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-4"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-5"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-6"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-7"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-8"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-9"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-10"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-11"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-12"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-13"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-14"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-15"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-16"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-17"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-18"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-19"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$tempo_compra_aprovacao == "-20"] <- "aprovacao antecipada" # nolint
    result$periodo_compra_aprovacao[result$periodo_compra_aprovacao == " "] <- "mais de 20 dias" # nolint
    result$periodo_compra_aprovacao[result$periodo_compra_aprovacao == ""] <- "mais de 20 dias" # nolint

result$periodo_aprovacao_envio <- ""
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "0"] <- "envio imediato" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "1"] <- "1 a 5 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "2"] <- "1 a 5 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "3"] <- "1 a 5 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "4"] <- "1 a 5 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "5"] <- "1 a 5 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "6"] <- "5 a 10 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "7"] <- "5 a 10 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "8"] <- "5 a 10 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "9"] <- "5 a 10 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "10"] <- "5 a 10 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "11"] <- "10 a 15 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "12"] <- "10 a 15 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "13"] <- "10 a 15 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "14"] <- "10 a 15 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "15"] <- "10 a 15 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "16"] <- "15 a 20 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "17"] <- "15 a 20 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "18"] <- "15 a 20 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "19"] <- "15 a 20 dias" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "20"] <- "15 a 20 dias" # nolint

    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-1"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-2"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-3"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-4"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-5"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-6"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-7"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-8"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-9"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-10"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-11"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-12"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-13"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-14"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-15"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-16"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-17"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-18"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-19"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$tempo_aprovacao_envio == "-20"] <- "aprovacao antecipada" # nolint
    result$periodo_aprovacao_envio[result$periodo_aprovacao_envio == " "] <- "mais de 20 dias" # nolint
    result$periodo_aprovacao_envio[result$periodo_aprovacao_envio == ""] <- "mais de 20 dias" # nolint

result$periodo_envio_entrega <- ""
    result$periodo_envio_entrega[result$tempo_envio_entrega == "1"] <- "1 a 5 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "2"] <- "1 a 5 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "3"] <- "1 a 5 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "4"] <- "1 a 5 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "5"] <- "1 a 5 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "6"] <- "5 a 10 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "7"] <- "5 a 10 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "8"] <- "5 a 10 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "9"] <- "5 a 10 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "10"] <- "5 a 10 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "11"] <- "10 a 15 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "12"] <- "10 a 15 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "13"] <- "10 a 15 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "14"] <- "10 a 15 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "15"] <- "10 a 15 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "16"] <- "15 a 20 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "17"] <- "15 a 20 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "18"] <- "15 a 20 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "19"] <- "15 a 20 dias" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "20"] <- "15 a 20 dias" # nolint

    result$periodo_envio_entrega[result$tempo_envio_entrega == "-1"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-2"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-3"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-4"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-5"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-6"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-7"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-8"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-9"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-10"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-11"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-12"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-13"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-14"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-15"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-16"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-17"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-18"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-19"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$tempo_envio_entrega == "-20"] <- "entrega antecipada" # nolint
    result$periodo_envio_entrega[result$periodo_envio_entrega == " "] <- "mais de 20 dias" # nolint
    result$periodo_envio_entrega[result$periodo_envio_entrega == ""] <- "mais de 20 dias" # nolint

result$periodo_total_venda <- ""
    result$periodo_total_venda[result$tempo_total_venda == "1"] <- "1 a 5 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "2"] <- "1 a 5 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "3"] <- "1 a 5 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "4"] <- "1 a 5 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "5"] <- "1 a 5 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "6"] <- "5 a 10 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "7"] <- "5 a 10 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "8"] <- "5 a 10 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "9"] <- "5 a 10 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "10"] <- "5 a 10 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "11"] <- "10 a 15 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "12"] <- "10 a 15 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "13"] <- "10 a 15 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "14"] <- "10 a 15 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "15"] <- "10 a 15 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "16"] <- "15 a 20 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "17"] <- "15 a 20 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "18"] <- "15 a 20 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "19"] <- "15 a 20 dias" # nolint
    result$periodo_total_venda[result$tempo_total_venda == "20"] <- "15 a 20 dias" # nolint
    result$periodo_total_venda[result$periodo_total_venda == " "] <- "mais de 20 dias" # nolint
    result$periodo_total_venda[result$periodo_total_venda == ""] <- "mais de 20 dias" # nolint

unique(result$tempo_data_limite)
result$periodo_data_limite <- ""
    result$periodo_data_limite[result$tempo_data_limite == "1"] <- "1 a 5 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "2"] <- "1 a 5 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "3"] <- "1 a 5 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "4"] <- "1 a 5 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "5"] <- "1 a 5 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "6"] <- "5 a 10 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "7"] <- "5 a 10 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "8"] <- "5 a 10 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "9"] <- "5 a 10 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "10"] <- "5 a 10 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "11"] <- "10 a 15 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "12"] <- "10 a 15 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "13"] <- "10 a 15 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "14"] <- "10 a 15 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "15"] <- "10 a 15 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "16"] <- "15 a 20 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "17"] <- "15 a 20 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "18"] <- "15 a 20 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "19"] <- "15 a 20 dias" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "20"] <- "15 a 20 dias" # nolint
    result$periodo_data_limite[result$periodo_data_limite == " "] <- "mais de 20 dias" # nolint
    result$periodo_data_limite[result$periodo_data_limite == ""] <- "mais de 20 dias" # nolint
    
    result$periodo_data_limite[result$tempo_data_limite == "-1"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-2"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-3"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-4"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-5"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-6"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-7"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-8"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-9"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-10"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-11"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-12"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-13"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-14"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-15"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-16"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-17"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-18"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-19"] <- "dentro da data limite" # nolint
    result$periodo_data_limite[result$tempo_data_limite == "-20"] <- "dentro da data limite" # nolint

result$prazo_status <- stringr::str_remove(result$tempo_data_limite, pattern = "(//s+[A-Za-z]+)?[0-9]+") # nolint
    result$prazo_status[result$prazo_status == ""] <- "dentro do prazo"
    result$prazo_status[result$prazo_status == "-"] <- "fora do prazo"

result <- estate_brasil_mes_aprovacao(result)
result <- estate_brasil_mes_envio(result)
result <- estate_brasil_mes_estimado(result)
result <- estate_brasil_mes_entrega(result)]


openxlsx::write.xlsx(result, file = "D:/green_lake/Projetos/e-commerce/resultado/e_commerce_brasil.xlsx") # nolint