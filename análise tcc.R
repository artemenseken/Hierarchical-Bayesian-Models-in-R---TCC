#setInternet2(TRUE)
#options(repos='http://cran.ma.imperial.ac.uk/')
#.libPaths(c("C:\\Users\\e.arthur.machado\\Desktop\\R", .libPaths()))
#setwd("C:\\Users\\e.arthur.machado\\Desktop\\Análise TCC")
#vendo os pdf
library(devtools)
library(shinystan)
library(glmer2stan)
library(rstan)
library(lmerTest)
source('mypostplot.R')
source('myglmer2stan.R')
source('panelfuns.R')
source('postPairs.R')
source('stanmer2.R')
source('propInRope.R')
library("brms")
library(gridExtra)
require(car)
library(processx)
#É A LC 62/89 QUE FALA SOBRE O REPASSE DE VERBAS

require(openxlsx)
require(ggplot2)
data<-read.xlsx(file.choose(),sheet=1)# importar o banco Cópia de Dados relatório Resgate a Infância
colnames(data)
data<-data[,-c(2,4,7,8,10,11:13,15,16,27,28:30,32:40,46,50:55,59:74,75:77)]

data$Unid.básica_saúdeset2017<-as.numeric(gsub("-","0",data$Unid.básica_saúdeset2017))
data$Profes_EnsinoMédio<-as.numeric(gsub("-","0",data$Profes_EnsinoMédio))

data$PopEstimada2017[211]<-"8059"
data$PopEstimada2017[1347]<-"7386"
data$PopEstimada2017[1455]<-"15276"
data$PopEstimada2017[1937]<-"16814"
data$PopEstimada2017<-as.numeric(data$PopEstimada2017)
data$IDH2010<-as.numeric(data$IDH2010)
data$IDHM2010<-as.numeric(data$IDHM2010)

data$Alunos_por_professor<-(data$MatriAnosIniciais+data$MatriAnosFinais+data$MatriEnsinoMedio)/(data$Profes_AnosFinais+data$Profes_AnosInicias+data$Profes_EnsinoMédio)
data$UBS_por_Pessoas<-data$Unid.básica_saúdeset2017/data$PopEstimada2017#ver por mil habitantes, tive q fazer isso pois tem casos que tem zero

data<-data[,-c(8:16,26:28)]

dados.dos.municipios<-read.xlsx(file.choose(),sheet=1)#importar o banco FPM dos municípios

colnames(dados.dos.municipios)[1]<-"NomeMunic"
novis<-merge(data,dados.dos.municipios,by.x="NomeMunic")

meus_dados_completos_com_repasse<-novis[!duplicated(novis$CodMunicCompleto),]
meus_dados_completos_com_repasse<-meus_dados_completos_com_repasse[,-c(2,6,9,11:16,19:21)]
meus_dados_completos_com_repasse$indice<-c(1:dim(meus_dados_completos_com_repasse)[1])

tei<-meus_dados_completos_com_repasse[-c(472,3113,3577,3757,3817),]#foram retirados as observações por falta de informação: 472 - Balneário Rincão, 3113 - Mojuí dos Campos, 3577 -Paraíso das Águas, 3757 - Pescaria Brava e 3817 - Pinto Bandeira
which(! complete.cases(tei))#verificar se tem NAs no banco, quando tem isso atrapalha a regressão, ele omite a linha 
tei$MunicCofinanciado<-as.factor(tei$MunicCofinanciado)
tei$NomeMunic<-as.factor(tei$NomeMunic)
tei$NomeUF<-as.factor(tei$NomeUF)

tei$Repasse<-log(tei$Repasse)
tei$PopEstimada2017<-log(tei$PopEstimada2017)
tei$PIBdoMunicipio<-log(tei$PIBdoMunicipio)
tei$UBS_por_Pessoas<-tei$UBS_por_Pessoas*1000#para mil pessoas

tei<-tei[,-c(11)]
tei[is.na(tei$UBS_por_Pessoas),]
tei[4235,9]=0

############################# ANÁLISE DESCRITIVA ######################################
#Variável Repasse
summary(tei$Repasse)
sd(tei$Repasse)
ggplot(tei,aes(x=Repasse))+geom_histogram(colour="white",
                                      fill="#A11D21",bins=12)+
  labs(x="Log do repasse de verba federal",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
ggsave("hist_repasse.png",width=158,height=93,units="mm")

#outras variáveis
a<-ggplot(tei,aes(x=MunicCofinanciado))+geom_bar(fill="#A11D21")+
  labs(x="Município cofinanciado",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))+
  coord_flip()

b<-ggplot(tei,aes(x=PopEstimada2017))+geom_histogram(colour="white",
                                             fill="#A11D21",bins=12)+
  labs(x="Log da população estimada",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
c<-ggplot(tei,aes(x=IDHM2010))+geom_histogram(colour="white",
                                                     fill="#A11D21",bins=12)+
  labs(x="IDH do município",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
d<-ggplot(tei,aes(x=TaxaTrab10a17))+geom_histogram(colour="white",
                                              fill="#A11D21",bins=12)+
  labs(x="Taxa de trabalho infantil",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
e<-ggplot(tei,aes(x=PIBdoMunicipio))+geom_histogram(colour="white",
                                                   fill="#A11D21",bins=12)+
  labs(x="Log do PIB do município",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
f<-ggplot(tei,aes(x=Alunos_por_professor))+geom_histogram(colour="white",
                                                    fill="#A11D21",bins=12)+
  labs(x="Alunos por professor",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
g<-ggplot(tei,aes(x=UBS_por_Pessoas))+geom_histogram(colour="white",
                                                          fill="#A11D21",bins=12)+
  labs(x="UBS para cada mil pessoas",y="Frequência")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour="black"))
grid.arrange(a,b,c,d,e,f,g)
ggsave("variaveis_descritiva.png",width=200,height=120,units="mm")

ggplot(tei,aes(x=NomeUF,y=Repasse))+
  geom_boxplot(fill=c("#A11D21"),width=0.5)+
  stat_summary(fun.y="mean",geom="point",shape=23,size=3,
               fill="white")+
  labs(x="",y="Log do repasse de verba federal")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black",size=12),
        axis.title.x=element_text(colour="black",size=12),
        axis.text=element_text(colour="black",size=9.5,angle = 45, hjust = 1),
        panel.border=element_blank(),
        axis.line.y=element_line(colour="black"))
ggsave("boxplot_repasse.png",width=200,height=120,units="mm")

cor(tei[,-c(1,2,3)])
####################################### MODELAGEM #################################################
##MODELOS CLÁSSICOS

#Modelo Nulo
modeloNulo<-lmer(Repasse ~ 1 + (1 | NomeUF), data=tei)
summary(modeloNulo)

View(cor(bancodedados.paramodeloclassico1[,-1]))
#MODELO COM EFEITOS FIXOS
bancodedados.paramodeloclassico1<-tei[,-c(1:2)]
modeloclassico1<-lm(Repasse ~ .,data=bancodedados.paramodeloclassico1)
summary(modeloclassico1)
modeloclassico1$fitted.values

a<-predict(lm(Repasse ~ PIBdoMunicipio,bancodedados.paramodeloclassico1),bancodedados.paramodeloclassico1)
a<-data.frame(predito=predict(lm(Repasse ~ .,data=bancodedados.paramodeloclassico1),bancodedados.paramodeloclassico1))

ggplot(bancodedados.paramodeloclassico1,aes(x=PIBdoMunicipio,y=Repasse))+
  geom_point()+
  guides(color=F)+
  geom_line(data=bancodedados.paramodeloclassico1,aes(x=PIBdoMunicipio,y=a$predito),colour="blue")#fez a regressão individual


ggplot(bancodedados.paramodeloclassico1,aes(x=TaxaTrab10a17,y=Repasse))+
  geom_point()+
  guides(color=F)+
  geom_line(data=bancodedados.paramodeloclassico1,aes(x=TaxaTrab10a17,y=a$predito),colour="blue")

par(mfrow=c(1,3))
plot(sort(predict(modeloclassico1)),sort(bancodedados.paramodeloclassico1$Repasse),xlab="Valores preditos",ylab="Valores observados",main="Modelo 1")
abline(0,1, lty = 1, col = "red",lwd=2)


#análise de resíduos para o primeiro, fazendo para o primeiro podemos generalizar para os outros
library(nortest)
library("lmtest")
t1 <- ks.test(modeloclassico1$residuals, "pnorm", mean(modeloclassico1$residuals), sd(modeloclassico1$residuals)) # KS
t2 <- lillie.test(modeloclassico1$residuals) # Lilliefors
t3 <- cvm.test(modeloclassico1$residuals) # Cramér-von Mises
t4 <- shapiro.test(modeloclassico1$residuals) # Shapiro-Wilk
t5 <- sf.test(modeloclassico1$residuals) # Shapiro-Francia
t6 <- ad.test(modeloclassico1$residuals)

qqnorm(modeloclassico1$residuals)
qqline(modeloclassico1$residuals, lty = 2, col = "red")

bptest(modeloclassico1, data = bancodedados.paramodeloclassico1)

vif(modeloclassico1)#tem um vif alto algumas variáveis

#MODELO COM INTERCEPTOS ALEATÓRIOS

require(plotly)
bancodedados.paramodeloclassico2<-tei[,-c(1)]

options(scipen=999)
require(lme4)
require(lmerTest)

modeloclassico2<-lmer(Repasse ~ .+(1|NomeUF),data=bancodedados.paramodeloclassico2)

summary(modeloclassico2)

ggplot(bancodedados.paramodeloclassico2, aes(x = PIBdoMunicipio,y=Repasse,color=NomeUF,group=NomeUF)) +
  geom_point() +
  geom_line(aes(y = predict(modeloclassico2)),size=1)

plot(sort(predict(modeloclassico2)),sort(bancodedados.paramodeloclassico1$Repasse),xlab="Valores preditos",ylab="Valores observados",main="Modelo 2")
abline(0,1, lty = 1, col = "red",lwd=2)

#MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIAS

require(nlme)

bancodedados.paramodeloclassico3<-tei[,-c(1)]

news2<-data.frame(Repasse=bancodedados.paramodeloclassico3$Repasse,NomeUF=bancodedados.paramodeloclassico3$NomeUF,IDHM2010=bancodedados.paramodeloclassico3$IDHM2010,PopEstimada2017=bancodedados.paramodeloclassico3$PopEstimada2017,MunicCofinanciado=bancodedados.paramodeloclassico3$MunicCofinanciado,
                  TaxaTrab10a17=bancodedados.paramodeloclassico3$TaxaTrab10a17,PIBdoMunicipio=bancodedados.paramodeloclassico3$PIBdoMunicipio,Alunos_por_professor=bancodedados.paramodeloclassico3$Alunos_por_professor,UBS_por_Pessoas=bancodedados.paramodeloclassico3$UBS_por_Pessoas)

new.data.grp <- groupedData(Repasse ~  PIBdoMunicipio | NomeUF,news2)

modeloclassico3ab<-lme(Repasse ~ MunicCofinanciado+PopEstimada2017+IDHM2010+TaxaTrab10a17+PIBdoMunicipio+Alunos_por_professor+UBS_por_Pessoas,
                       random = ~ MunicCofinanciado+PopEstimada2017+IDHM2010+TaxaTrab10a17+PIBdoMunicipio+Alunos_por_professor+UBS_por_Pessoas | NomeUF,control=lmeControl(opt="optim",maxIter=50000,msMaxIter=200,tolerance=1e-4,niter=50,msTol=1e-5,nlmSepMax=500,msVerbose=T,returnObject = T),new.data.grp)

summary(modeloclassico3ab)
modeloclassico3ab$coefficients

plot(augPred(modeloclassico3ab))#gráfico massa demais, aqui eu rodo o que está no groupedData, se eu quiser comparar com outra variável eu tenho que mudar lá no GroupedData

plot(ranef(modeloclassico3ab))

plot(sort(predict(modeloclassico3ab)),sort(bancodedados.paramodeloclassico1$Repasse),xlab="Valores preditos",ylab="Valores observados",main="Modelo 3")
abline(0,1, lty = 1, col = "red",lwd=2)
#SELEÇÃO DE MODELOS 
require(bbmle)

AICctab(modeloNulo,base=T,weights=T)
BICtab(modeloNulo,base=T,weights=T)

AICctab(modeloclassico1,modeloclassico2,modeloclassico3ab,base=T,weights=T)
AICtab(modeloNulo,modeloclassico1,modeloclassico2,modeloclassico3ab,base=T,weights=T)
BICtab(modeloclassico1,modeloclassico2,modeloclassico3ab,base=T,weights=T)

##MODELOS BAYESIANOS

#MODELO COM EFEITOS FIXOS

repasse=news2$Repasse
IDHM2010=news2$IDHM2010
PopEstimada2017=news2$PopEstimada2017
MunicCofinanciado=as.numeric(as.factor(news2$MunicCofinanciado))
TaxaTrab10a17=news2$TaxaTrab10a17
PIBdoMunicipio=news2$PIBdoMunicipio
Alunos_por_professor=news2$Alunos_por_professor
UBS_por_Pessoas=news2$UBS_por_Pessoas 
NomeUF=news2$NomeUF

earn_dat <- list(N = 5562 ,
                 repasse = repasse,
                 IDHM2010=news2$IDHM2010,
                 PopEstimada2017=PopEstimada2017,
                 MunicCofinanciado=MunicCofinanciado,
                 TaxaTrab10a17=TaxaTrab10a17,
                 PIBdoMunicipio=PIBdoMunicipio,
                 Alunos_por_professor=Alunos_por_professor,
                 UBS_por_Pessoas=UBS_por_Pessoas
)

modelobayesiano1 = "data{
int<lower=0>  N; // Número de observações
vector[N] repasse; // variável resposta
vector[N] PopEstimada2017; // preditor
vector[N] MunicCofinanciado; // preditor
vector[N] TaxaTrab10a17; // preditor
vector[N] PIBdoMunicipio; // preditor
vector[N] IDHM2010; // preditor
vector[N] Alunos_por_professor; // preditor
vector[N] UBS_por_Pessoas; // preditor
}
parameters{
vector[7] beta; // intercepto e inclinações
vector[1] intercepto;
real<lower=0> sigma; // erro padrão
}
model{
real mu;
for (i in 1:N){
mu=intercepto[1]+beta[1]*PIBdoMunicipio[i]+beta[2]*PopEstimada2017[i]+beta[3]*MunicCofinanciado[i]+
beta[4]*TaxaTrab10a17[i]+beta[5]*IDHM2010[i]+beta[6]*Alunos_por_professor[i]+beta[7]*UBS_por_Pessoas[i]; // verossimilhança
repasse[i] ~ normal (mu,sigma);

}
}
generated quantities{
  real dev;
  dev<-0;
  for(n in 1:N){
    dev<-dev+(-2)*normal_log(repasse[n],intercepto[1]+beta[1]*PIBdoMunicipio[n]+beta[2]*PopEstimada2017[n]+beta[3]*MunicCofinanciado[n]+
                               beta[4]*TaxaTrab10a17[n]+beta[5]*IDHM2010[n]+beta[6]*Alunos_por_professor[n]+beta[7]*UBS_por_Pessoas[n],sigma); // calculo do deviance
  }
}
"

fit2 <- stan(model_code = modelobayesiano1, data = earn_dat,
             warmup = 1000,
             iter = 10000, 
             chains = 5)
print(fit2)

fit2_samples = extract(fit2)
str(fit2_samples)

ggplot() + geom_point(data=news2,aes(x=news2$PopEstimada2017,y=repasse)) + 
  geom_abline(slope=mean(fit2_samples$beta[,2]),intercept=mean(fit2_samples$intercepto),size=1,color='red')+
  geom_abline(intercept=11.437688,slope=0.4552630,color='blue')

traceplot(fit2)

stan_dens(fit2)
stan_hist(fit2)

### calculo do EAIC, EBIC, WAIC E DIC
#Obs.: o lp___ é o log likelihood,https://www.jax.org/news-and-insights/jax-blog/2015/october/lp-in-stan-output
#EBIC
EBIC=mean(fit2_samples$dev)+9*log(dim(news2)[1])#o 8 é a quantidade de parâmetros
#EAIC
EAIC=mean(fit2_samples$dev)+2*9#o 8 é a quantidade de parâmetros
#DIC
pd=sum(-2*dnorm(repasse, mean(fit2_samples$intercepto)+ mean(fit2_samples$beta[,1])*PIBdoMunicipio+mean(fit2_samples$beta[,2])*PopEstimada2017+
               mean(fit2_samples$beta[,3])*MunicCofinanciado+mean(fit2_samples$beta[,4])*TaxaTrab10a17+mean(fit2_samples$beta[,5])*IDHM2010+
               mean(fit2_samples$beta[,6])*Alunos_por_professor+mean(fit2_samples$beta[,7])*UBS_por_Pessoas, mean(fit2_samples$sigma), log=T)) 
DIC=pd+mean(fit2_samples$dev)

#MODELO OCOM INTERCEPTOS ALEATÓRIOS, na verdade são as inclinações aleatórias? no pdf tá como interceptos, ver isso ai, no site lá em baixo ele faz um com interceptos aleatorios e outro com inclinações aleatórias

earn_dat <- list(N = 5562 , #specify number of observations as a scalar
                 repasse = repasse, # data vector
                 IDHM2010=news2$IDHM2010,
                 PopEstimada2017=PopEstimada2017,
                 MunicCofinanciado=MunicCofinanciado,
                 TaxaTrab10a17=TaxaTrab10a17,
                 PIBdoMunicipio=PIBdoMunicipio,
                 Alunos_por_professor=Alunos_por_professor,
                 UBS_por_Pessoas=UBS_por_Pessoas,
                 NomeUF=as.integer(NomeUF),
                 J=26
)

modelobayesiano2 = "data{
int<lower=1>  N; // Número de observações
int<lower=1> J;
int <lower=1,upper= J> NomeUF[N]; //identificação dos estados
vector[N] repasse; // variável resposta
vector[N] PopEstimada2017; // preditor
vector[N] MunicCofinanciado; // preditor
vector[N] TaxaTrab10a17; // preditor
vector[N] PIBdoMunicipio; // preditor
vector[N] IDHM2010; // preditor
vector[N] Alunos_por_professor; // preditor
vector[N] UBS_por_Pessoas; // preditor
}
parameters{
vector[8] beta; // intercepto e inclinações
vector[J] w; // intercepto de estados
real<lower=0> sigma_e; // erro padrão
real<lower=0> sigma_w; // componentes de variância dos municípios

}
model{
real mu;
w ~ normal(0,sigma_w); // priori do efeito aleatório do estado
for (n in 1:N){
mu=beta[1]+w[NomeUF[n]]+beta[2]*PIBdoMunicipio[n]+beta[3]*PopEstimada2017[n]+beta[4]*MunicCofinanciado[n]+
beta[5]*TaxaTrab10a17[n]+beta[6]*IDHM2010[n]+beta[7]*Alunos_por_professor[n]+beta[8]*UBS_por_Pessoas[n]; // verossimilhança
repasse[n] ~ normal (mu,sigma_e);
}
}
generated quantities{
  real dev;
  dev<-0;
  for(i in 1:N){
    dev<-dev+(-2)*normal_log(repasse[i],beta[1]+w[NomeUF[i]]+beta[2]*PIBdoMunicipio[i]+beta[3]*PopEstimada2017[i]+beta[4]*MunicCofinanciado[i]+
                               beta[5]*TaxaTrab10a17[i]+beta[6]*IDHM2010[i]+beta[7]*Alunos_por_professor[i]+beta[8]*UBS_por_Pessoas[i],sigma_e); // calculo do deviance
  }
}
" 

fit3 <- stan(model_code = modelobayesiano2, data = earn_dat,
             warmup = 400,
             iter = 4000, 
             chains = 4)

print(fit3)

fit3_samples = extract(fit3)

qplot(PopEstimada2017,repasse)+geom_smooth(method=lm,se=F)+geom_abline(intercept=mean(fit3_samples$beta[,1]),slope=mean(fit3_samples$beta[,4]),color='red')# bem próximo

yestimado=mean(fit3_samples$beta[,1])+mean(fit3_samples$beta[,2])*

traceplot(fit3)

stan_hist(fit3)
stan_dens(fit3)

### calculo do EAIC, EBIC, WAIC E DIC
#EBIC
EBIC=mean(fit3_samples$dev)+35*log(dim(news2)[1])#o 35 é a quantidade de parâmetros, será?
#EAIC
EAIC=mean(fit3_samples$dev)+2*35#o 35 é a quantidade de parâmetros
#DIC
pd=sum(-2*dnorm(repasse, mean(fit3_samples$intercepto)+ mean(fit3_samples$beta[,1])*PIBdoMunicipio+mean(fit3_samples$beta[,2])*PopEstimada2017+
                  mean(fit3_samples$beta[,3])*MunicCofinanciado+mean(fit3_samples$beta[,4])*TaxaTrab10a17+mean(fit3_samples$beta[,5])*IDHM2010+
                  mean(fit3_samples$beta[,6])*Alunos_por_professor+mean(fit3_samples$beta[,7])*UBS_por_Pessoas, mean(fit3_samples$sigma), log=T)) 
DIC=pd+mean(fit3_samples$dev)

new_x<-data.frame(x1=new_X[,2],x2=rep(c("Min","Mean","Max"),each=20))


qplot(PopEstimada2017,repasse)+geom_smooth(method=lm,se=F)+geom_abline(intercept=int,slope=slope,color='darkred')+geom_abline(intercept=int+mean(fit3_samples$w[,1]),slope=slope,color='green')+geom_abline(intercept=int+mean(fit3_samples$w[,2]),slope=slope,color='yellow')+
  geom_abline(intercept=int+mean(fit3_samples$w[,3],size=3),slope=slope,color='pink')+geom_abline(intercept=int+mean(fit3_samples$w[,4]),slope=slope,color='purple')

#para o gráfico acima é a azul é a lm normal, a vermelha a reta do fit3 e a verde para o estado de Sao Paulo
#a ideia desse é que os interceptos vao ser diferentes

#MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIAS, ver nesse site:https://mc-stan.org/users/documentation/case-studies/radon.html, dá para me basear para fazer

earn_dat <- list(N = 5562 , #specify number of observations as a scalar
                 repasse = repasse, # data vector
                 IDHM2010=news2$IDHM2010,
                 PopEstimada2017=PopEstimada2017,
                 MunicCofinanciado=MunicCofinanciado,
                 TaxaTrab10a17=TaxaTrab10a17,
                 PIBdoMunicipio=PIBdoMunicipio,
                 Alunos_por_professor=Alunos_por_professor,
                 UBS_por_Pessoas=UBS_por_Pessoas,
                 NomeUF=as.integer(NomeUF),
                 J=26
)

#SELEÇÃO DE MODELOS BAYESIANOS

varying_intercept_slope = "
data {
int<lower=0>  N; // Número de observações
int<lower=1> J;
vector[N] repasse; // variável resposta
vector[N] PopEstimada2017; // preditor
vector[N] MunicCofinanciado; // preditor
vector[N] TaxaTrab10a17; // preditor
vector[N] PIBdoMunicipio; // preditor
vector[N] IDHM2010; // preditor
vector[N] Alunos_por_professor; // preditor
vector[N] UBS_por_Pessoas; // preditor
int NomeUF[N];
}
parameters {
real<lower=0> sigma;
real<lower=0> sigma_intercepto;
real<lower=0> sigma_beta1;
real<lower=0> sigma_beta2;
real<lower=0> sigma_beta3;
real<lower=0> sigma_beta4;
real<lower=0> sigma_beta5;
real<lower=0> sigma_beta6;
real<lower=0> sigma_beta7;
vector[J] intercepto;
vector[J] beta1;
vector[J] beta2;
vector[J] beta3;
vector[J] beta4;
vector[J] beta5;
vector[J] beta6;
vector[J] beta7;
real mu_intercepto;
real mu_beta1;
real mu_beta2;
real mu_beta3;
real mu_beta4;
real mu_beta5;
real mu_beta6;
real mu_beta7;
}

model {
intercepto ~ normal(mu_intercepto, sigma_intercepto);
beta1 ~ normal(mu_beta1, sigma_beta1);
beta2 ~ normal(mu_beta2, sigma_beta2);
beta3 ~ normal(mu_beta3, sigma_beta3);
beta4 ~ normal(mu_beta4, sigma_beta4);
beta5 ~ normal(mu_beta5, sigma_beta5);
beta6 ~ normal(mu_beta6, sigma_beta6);
beta7 ~ normal(mu_beta7, sigma_beta7);
repasse ~ normal(intercepto[NomeUF] + beta1[NomeUF].*PopEstimada2017+
beta2[NomeUF].*MunicCofinanciado+beta3[NomeUF].*TaxaTrab10a17+
beta4[NomeUF].*PIBdoMunicipio+beta5[NomeUF].*IDHM2010+beta6[NomeUF].*Alunos_por_professor+
beta7[NomeUF].*UBS_por_Pessoas, sigma);
}
"

############### área de testes #########################

fit5 <- stan(model_code = varying_intercept_slope, data = earn_dat,
             warmup = 400,
             iter = 4000, 
             chains = 4)
#deu certo
print(fit5)

fit5_samples = extract(fit5)
str(fit5_samples)

traceplot(fit5,pars = c("mu_intercepto","mu_beta1","mu_beta2","mu_beta3","mu_beta4","mu_beta5","mu_beta6","mu_beta7","sigma"))

stan_dens(fit5,pars = c("mu_intercepto","mu_beta1","mu_beta2","mu_beta3","mu_beta4","mu_beta5","mu_beta6","mu_beta7","sigma"))
stan_hist(fit5,pars = c("mu_intercepto","mu_beta1","mu_beta2","mu_beta3","mu_beta4","mu_beta5","mu_beta6","mu_beta7","sigma"))

### calculo do EAIC, EBIC, WAIC E DIC
#EBIC
EBIC=mean(fit5_samples$dev)+45*log(dim(news2)[1])#o 45 é a quantidade de parâmetros, será?
#EAIC
EAIC=mean(fit5_samples$dev)+2*45#o 45 é a quantidade de parâmetros
#DIC
pd=sum(-2*dnorm(repasse, mean(fit5_samples$intercepto)+ mean(fit5_samples$beta[,1])*PIBdoMunicipio+mean(fit5_samples$beta[,2])*PopEstimada2017+
                  mean(fit5_samples$beta[,3])*MunicCofinanciado+mean(fit5_samples$beta[,4])*TaxaTrab10a17+mean(fit5_samples$beta[,5])*IDHM2010+
                  mean(fit5_samples$beta[,6])*Alunos_por_professor+mean(fit5_samples$beta[,7])*UBS_por_Pessoas, mean(fit5_samples$sigma), log=T)) 
DIC=pd+mean(fit5_samples$dev)

# subset just the betas
beta.sigma = fit5_samples$sigma
beta.sigma_intercepto = fit5_samples$sigma_intercepto
beta.sigma_beta1 = fit5_samples$sigma_beta1
beta.intercepto = fit5_samples$intercepto
beta.beta1 = fit5_samples$beta1
beta.beta2 = fit5_samples$beta2
beta.beta3 = fit5_samples$beta3
beta.beta4 = fit5_samples$beta4
beta.beta5 = fit5_samples$beta5
beta.beta6 = fit5_samples$beta6
beta.beta7 = fit5_samples$beta7
beta.mu_intercepto = fit5_samples$mu_intercepto
beta.mu_beta1 = fit5_samples$mu_beta1
beta.mu_beta2 = fit5_samples$mu_beta2
beta.mu_beta3 = fit5_samples$mu_beta3
beta.mu_beta4 = fit5_samples$mu_beta4
beta.mu_beta5 = fit5_samples$mu_beta5
beta.mu_beta6 = fit5_samples$mu_beta6
beta.mu_beta7 = fit5_samples$mu_beta7
beta.lp = fit5_samples$lp__

nossa<-data.frame(table(tei$NomeUF))
intercept<-NULL
PopEstimada2017<-NULL
MunicCofinanciado<-NULL
TaxaTrab10a17<-NULL
PIBdoMunicipio<-NULL
IDHM2010<-NULL
Alunos_por_professor<-NULL
UBS_por_Pessoas<-NULL
for(i in 1:26){
  intercept[i]<-mean(beta.intercepto[,i])
  PopEstimada2017[i]<-mean(beta.beta1[,i])
  MunicCofinanciado[i]<-mean(beta.beta2[,i])
  TaxaTrab10a17[i]<-mean(beta.beta3[,i])
  PIBdoMunicipio[i]<-mean(beta.beta4[,i])
  IDHM2010[i]<-mean(beta.beta5[,i])
  Alunos_por_professor[i]<-mean(beta.beta6[,i])
  UBS_por_Pessoas[i]<-mean(beta.beta7[,i])
}
betas<-data.frame(intercept,PopEstimada2017,MunicCofinanciado,TaxaTrab10a17,PIBdoMunicipio,IDHM2010,Alunos_por_professor,UBS_por_Pessoas,group=as.factor(nossa$Var1))

write.csv2(betas,"C:\\Users\\e.arthur.machado\\Desktop\\Análise TCC\\betas do modelo bayesiano 3.csv")#escrevi os resultados para depois mostrar para o professor e não precisar rodar o programa

betas<-read.csv2(file.choose())#importar o arquivo betas do modelo bayesiano 3.csv
library(ggplot2)
#fazendo para cada variável
ggplot() + geom_point(data=news2,aes(x=PopEstimada2017,y=repasse)) + 
  geom_abline(data=betas,aes(slope=PopEstimada2017,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")

ggplot() + geom_point(data=news2,aes(x=MunicCofinanciado,y=repasse)) + 
  geom_abline(data=betas,aes(slope=MunicCofinanciado,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")

ggplot() + geom_point(data=news2,aes(x=TaxaTrab10a17,y=repasse)) + 
  geom_abline(data=betas,aes(slope=TaxaTrab10a17,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")+
  scale_x_continuous(name="x", limits=c(0,80)) +
  scale_y_continuous(name="y", limits=c(0,30))

ggplot() + geom_point(data=news2,aes(x=PIBdoMunicipio,y=repasse)) + 
  geom_abline(data=betas,aes(slope=PIBdoMunicipio,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")+
  scale_x_continuous(name="x", limits=c(0,80)) +
  scale_y_continuous(name="y", limits=c(0,30))

ggplot() + geom_point(data=news2,aes(x=IDHM2010,y=repasse)) + 
  geom_abline(data=betas,aes(slope=IDHM2010,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")+
  scale_x_continuous(name="x", limits=c(0,80)) +
  scale_y_continuous(name="y", limits=c(0,30))

ggplot() + geom_point(data=news2,aes(x=Alunos_por_professor,y=repasse)) + 
  geom_abline(data=betas,aes(slope=Alunos_por_professor,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")+
  scale_x_continuous(name="x", limits=c(0,80)) +
  scale_y_continuous(name="y", limits=c(0,30))

ggplot() + geom_point(data=news2,aes(x=UBS_por_Pessoas,y=repasse)) + 
  geom_abline(data=betas,aes(slope=UBS_por_Pessoas,intercept=intercept,color=group),size=1)+geom_smooth(method=lm,se=F,colour="black")+
  scale_x_continuous(name="x", limits=c(0,80)) +
  scale_y_continuous(name="y", limits=c(0,30))


