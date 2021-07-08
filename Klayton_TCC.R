library(igraph) #Pacote para as redes
library(ffbase) # Pacote para leitura dos microdados
library(xlsx) #gravar em excel use antes R CMD javareconf -e

#Define o diretório de Trabalho (local onde está o arquivo DOCENTES_CO.CSV)
dir <- "/run/media/rafael/9baaa157-f581-49e1-a156-01965bfdb959/microdados/2020/DADOS/"
setwd(dir)
results <- data.frame(
        UF = c(),
        VERTICES = c(),
        ARESTAS = c(),
        DIAMETER = c(),
        MIN = c(),
        MAX = c(),
        MEAN = c(),
        SD = c(),
        PGMIN = c(),
        PGMAX = c(),
        PGMEAN = c(),
        PGSD = c())

regioes <- c("co", "nordeste", "norte", "sul", "sudeste")

par(mfrow=c(4,7))
par(mar=c(0, 0, 1, 0))

for(r in regioes) {
        filedocentes <- paste0("docentes_",r,".CSV")
        
        #Faz a leitura do DOCENTES_CO.CSV
        setwd(dir)
        docentes<-read.csv.ffdf(file=filedocentes,header=TRUE,sep="|",first.rows=10000000, colClasses=NA) 
        
        #Encontra os estados
        ufs <- unique(docentes$CO_UF)
        
        for(uf in 1:length(ufs)) {
                
                #Busca docentes que atuam na rede federal de ensino em cada estado
                id <- ffwhich(docentes, docentes$TP_DEPENDENCIA == 1 & docentes$CO_UF == ufs[uf])
                
                #Armazena os resultados de corumbá na variável dados
                dados <- docentes[id,][,]
                
                #Cria um data.frame para armazenar os dados de interesse
                s <- data.frame(ID_DOCENTE = c(), CO_IES = c())
                
                #Cria um novo data.frame somente com as colunas 2 e 36 do DOCENTES_CO.CSV (ID_DOCENTE e CO_IES_1)
                data <- subset(dados, select = c(ID_DOCENTE,CO_IES_1)) 
                
                #remove duplicados
                data <- unique(data) 
                names(data) <- c("ID_DOCENTE", "CO_IES")
                #Anexa os dados no data.frame criado
                s <- as.data.frame(as.ffdf(data))
                
                #Cria um novo data.frame somente com as colunas 2 e 36 do DOCENTES_CO.CSV (ID_DOCENTE e CO_IES_1)
                data <- subset(dados, select = c(ID_DOCENTE,CO_IES_2)) 
                names(data) <- c("ID_DOCENTE", "CO_IES")
                #remove duplicados
                data <- unique(data) 
                
                #Anexa os dados no data.frame criado
                s <- rbind(s, as.data.frame(as.ffdf(data)))
                
                #Cria um novo data.frame somente com as colunas 2 e 36 do DOCENTES_CO.CSV (ID_DOCENTE e CO_IES_1)
                data <- subset(dados, select = c(ID_DOCENTE,CO_IES_3)) 
                names(data) <- c("ID_DOCENTE", "CO_IES")
                #remove duplicados
                data <- unique(data) 
                
                #Anexa os dados no data.frame criado
                s <- rbind(s, as.data.frame(as.ffdf(data)))
                
                #Cria um novo data.frame somente com as colunas 2 e 126 do DOCENTES_CO.CSV (ID_DOCENTE e CO_ENTIDADE)
                data <- subset(dados, select = c(ID_DOCENTE,CO_ENTIDADE)) 
                
                #Remove LINHAS duplicadas
                data <- unique(data)
                
                #Cria um temporario
                tmp <- as.data.frame(as.ffdf(data))
                
                #Padroniza os nomes
                names(s) <- c("ID", "DE")
                names(tmp) <- c("ID", "DE")
                
                #Anexa os dados no data.frame criado
                s <- rbind(s, tmp)
                
                #Remove linhas com NA e remove duplicados
                s <- s[complete.cases(s), ]
                s <- unique(s)
                
                #cria a rede não direcionada
                g <- graph.data.frame(s, directed = F)
                
                pr <- page_rank(g)
                pr <- pr$vector
                dr <- degree(g)
                
                dat <- as.data.frame(pr)
                dr <- as.data.frame(dr)
                dat <- cbind(dat, dr[,1])
                dat <- cbind(rownames(dat), data.frame(dat, row.names=NULL))
                dat <- dat[c(1,3,2)]
                
                names(dat) <- c("vertex","degree", "pagerank")
                dat <- dat[order(dat$degree, decreasing = TRUE),]
                
                if(ufs[uf] == 11) { estado <- "RO" }
                if(ufs[uf] == 12) { estado <- "AC" }
                if(ufs[uf] == 13) { estado <- "AM" }
                if(ufs[uf] == 14) { estado <- "RR" }
                if(ufs[uf] == 15) { estado <- "PA" }
                if(ufs[uf] == 16) { estado <- "AP" }
                if(ufs[uf] == 17) { estado <- "TO" }
                if(ufs[uf] == 21) { estado <- "MA" }
                if(ufs[uf] == 22) { estado <- "PI" }
                if(ufs[uf] == 23) { estado <- "CE" }
                if(ufs[uf] == 24) { estado <- "RN" }
                if(ufs[uf] == 25) { estado <- "PB" }
                if(ufs[uf] == 26) { estado <- "PE" }
                if(ufs[uf] == 27) { estado <- "AL" }
                if(ufs[uf] == 28) { estado <- "SE" }
                if(ufs[uf] == 29) { estado <- "BA" }
                if(ufs[uf] == 31) { estado <- "MG" }
                if(ufs[uf] == 32) { estado <- "ES" }
                if(ufs[uf] == 33) { estado <- "RJ" }
                if(ufs[uf] == 35) { estado <- "SP" }
                if(ufs[uf] == 41) { estado <- "PR" }
                if(ufs[uf] == 42) { estado <- "SC" }
                if(ufs[uf] == 43) { estado <- "RS" }
                if(ufs[uf] == 50) { estado <- "MS" }
                if(ufs[uf] == 51) { estado <- "MT" }
                if(ufs[uf] == 52) { estado <- "GO" }
                if(ufs[uf] == 53) { estado <- "DF" }
                
                results <- rbind(results, c(estado,
                                            vcount(g),
                                            ecount(g),
                                            diameter(g),
                                            min(degree(g)),
                                            max(degree(g)),
                                            mean(degree(g)),
                                            sd(degree(g)),
                                            min(pr),
                                            max(pr),
                                            mean(pr),
                                            sd(pr)))
                print(estado)
                
                g.com <- fastgreedy.community(g)
                V(g)$color <- g.com$membership + 1
                
                plot(g,
                     vertex.size = 2,
                     vertex.label = NA,
                     layout = layout.kamada.kawai
                )
                title(estado) 
                
                setwd("/home/rafael/Documentos/IFDrive/Orientações/BFK/")
                #write.xlsx(dat[1:100,], "dados-final.xlsx", sheetName = estado, append = TRUE)  
        }
        
}

names(results) <- c("UF","VÉRTICES","LINKS","DIAMETRO","MIN","MÁX","MED.","SD","MIN","MÁX","MED.","SD")
setwd("/home/rafael/Documentos/IFDrive/Orientações/BFK/")
write.xlsx(results, "dados-final.xlsx", sheetName = "FULL", append = TRUE)  


