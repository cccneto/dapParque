			**********************************************************************
			*          MANIPULAÇAO DA BASE DE DADOS -    Lagoa Araça             *
			**********************************************************************

			import excel "~qaraca1.xlsx", sheet("dados") firstrow clear


			* TRANSFORMA VARIÁVEIS DO FORMATO TEXTO PARA O FORMATO NUMÉRICO
			destring q8, replace force
			destring q9, replace force
			destring q10, replace force
			destring q11, replace force

			**********************************************************************
			*                     ANÁLISE SOCIOECONÔMICA                         *
			**********************************************************************

			* GERA CLASSIFICAÇÕES PARA VARIÁVEIS RENDA DOMICILIAR, IDADE E ESCOLARIDADE 

			rename q1 idade
			generate idade2=idade^2
			recode q6 (1=0) (2 3=2) (4 5=3) (6 7=4) (8=5), gen(fescola)
			
			* Dummie "Visita" - "q7<=4" Significa visitar o parque ao menos 2 vezes por mes
			generate visit=.
			replace visit= 1 if q7<=4
			replace visit= 0 if q7>=5
			
			
			* "Sexo" 0= homem e 1= mulher
			rename q2 sexo
					
			**** CRIANDO DUMMIES PARA ESCOLARIDADE

			* Dummy para ensino superior
			gen escolaridade1=.
			replace escolaridade1 = 0 if q6<=6
			replace escolaridade1 = 1 if q6>=7
			
			**** CRIANDO DUMMIES PARA DISTANCIA 

			*Dummy para DISTANCIA do parque - implica que a pessoa mora a até 1km de distancia do parque
			generate distancia1=.
			replace distancia1 = 1 if q5<=3
			replace distancia1 = 0 if q5>=4
			
			*Visita o parque no final de semana?

			recode q8 (0=0) (1=1) (nonmissing=.), generate(finaldesemana)

			* TEMPO PERMANENCIA NO PARQUE

			generate tempo=q11
			gen income1=.
			replace income1 = income if income>=1 
			
			
		
			**********************************************************************
			*             GERANDO VARIAVEIS PARA ANALISE DA DAP 		 		 *
			**********************************************************************

			* GERA VARIÁVEIS DE ANÁLISE

			recode resdap1 (1=1) (0=0) (nonmissing=.), gen(aceita1)
			recode resdap2 (1=1) (0=0) (nonmissing=.), gen(aceita2)

			
			
			rename dap1 lance1
			rename dap2 lance2
			rename income1 Renda
			
			generate lnlance1= ln(lance1)
			generate lnlance2=ln(lance2)
			
			generate lndrenda= ln(Renda)
			generate lndrenda1= ln(Renda)
			
			generate t1=lance1*(-1)
			generate t2=lance2*(-1)
			gen lnRendat1 = ln(( Renda - t1)/ Renda)
			gen lnRendat2 = ln(( Renda - t2)/ Renda)

			
			label variable lance1 "Valor 1"
			label variable aceita1 "Aceita 1?"
			label variable lance2 "Valor 2"
			label variable aceita2 "Aceita 2?"

		
			
		*********************************************************************************

		* Bivariate Probit Model in Stata

	gen yy = aceita1 >=1 & aceita2 >=1
	gen yn = aceita1 >=1 & aceita2 <1
	gen nn = aceita1 <1 & aceita2 <1
	gen ny = aceita1 <1 & aceita2 >=1

	* Frequency distribution of double bound discrete response.
	tab yy
	tab yn
	tab ny
	tab nn
	sum yy yn nn ny


		* Descriptive stastistics of variables used in the analysis
		sum aceita1 aceita2 lance1 lance2 lnlance2 lnlance1 idade sexo Renda lnRendat1 lnRendat2 idade finaldesemana distancia1 tempo t1 t2

		* Analisando correlações, principalmente para os lances (1 e 2).
		cor lance1 lance2
		cor aceita1 aceita2
		cor aceita1 aceita2 lance1 lance2 lnlance2 lnlance1 idade Renda lnRendat1 lnRendat2 idade finaldesemana distancia1 tempo t1 t2
		
		*********************************************************************************************************
		*Criando variáveis com os valores médios
		
		egen aceita1_med = mean(aceita1)
		egen aceita2_med = mean(aceita2)		
		egen idade_med = mean(idade)
		egen escolaridade1_med = mean(escolaridade1)
		egen Renda_med = mean(Renda)
		egen finaldesemana_med = mean(finaldesemana)
		egen distancia1_med = mean(distancia1)
		egen tempo_med = mean(tempo)
		egen t1_med= mean(t1)
		egen sexo_med= mean(sexo)
		
		********** DEFININDO FORMAS FUNCIONAIS E SEUS RESPECTIVOS MODELOS **********************************


	
		* MODELO LINEAR NA RENDA - BIPROBITs
		********************************************************************

		* Definindo a lista de variaveis
		 global y1list aceita1
		 global y2list aceita2

		global hlist idade sexo escolaridade1 finaldesemana distancia1 tempo t1
 		global ilist idade sexo escolaridade1 finaldesemana distancia1 tempo t1
		
		
	 	describe $y1list $y2list $hlist 
		summarize $y1list $y2list $hlist
		
		tabulate $y1list $y2list 
		correlate $y1list $y2list 

		biprobit $y1list $y2list $hlist, nolog	
		gen DAP_biprobit_linear = (_coef[_cons] +_b[idade]*idade_med +_b[sexo]*sexo_med +_b[escolaridade1]*escolaridade1_med + _b[finaldesemana]*finaldesemana_med + _b[distancia1]*distancia1_med + _b[tempo]*tempo_med)/_b[t1]
		
		****************************************************************************************************************
		
		
				
			* MODELO LOG LINEAR NA RENDA - BIPROBITs
		********************************************************************
		* Definindo a lista de variaveis
		global y1list aceita1
		global y2list aceita2

		 global hlist idade sexo escolaridade1 finaldesemana distancia1 tempo lnRendat1
		 global ilist idade sexo escolaridade1 finaldesemana distancia1 tempo lnRendat1
			
		 biprobit $y1list $y2list $hlist,
		 gen DAPLogLinear_biprobit = Renda_med - Renda_med*(exp(-(_coef[_cons] +_b[idade]*idade_med + _b[sexo]*sexo_med +_b[escolaridade1]*escolaridade1_med + _b[finaldesemana]*finaldesemana_med + _b[distancia1]*distancia1_med + _b[tempo]*tempo_med)/_b[lnRendat1]))
	
		
		* Proporção da DAP estimada em função da Renda
		gen RDAPLL = (DAPLogLinear_biprobit / Renda_med)*100
		gen RDAPL = (DAP_biprobit_linear / Renda_med)*100
		
		sum RDAPLL RDAPL
		
	
	* Probabilidades marginais estimadas de y1=1 and y2=1
	 predict biprob1, pmarg1 
	 predict biprob2, pmarg2 

	* Probabilidade conjuntas estimadas de y1=0 and y2=0, y1=0 and y2=1, y1=1 and y2=0, and y1=1 and y2=1
	 predict biprob00, p00 
	 predict biprob01, p01 
	 predict biprob10, p10 
	 predict biprob11, p11 

	* Summarizing predicted values
	 summarize $y1list $y2list biprob1 biprob2 
	 summarize biprob00 biprob01 biprob10 biprob11
	 tabulate $y1list $y2list

	* Efeitos Marginais
	 margins, dydx(*) atmeans predict(p00)
	 margins, dydx(*) atmeans predict(p01)
	 margins, dydx(*) atmeans predict(p10)
	 margins, dydx(*) atmeans predict(p11)
	
	
	* O stata não está rodando os comandos abaixo e retorna a seguinte mensagem ( pmarg1 biprob1 already defined)
	******************************************************************************************************
		 
									
	* Probabilidades marginais estimadas de y1=1 and y2=1
	 predict biprob1, pmarg1 
	 predict biprob2, pmarg2 

	* Probabilidade conjuntas estimadas de y1=0 and y2=0, y1=0 and y2=1, y1=1 and y2=0, and y1=1 and y2=1
	 predict biprob00, p00 
	 predict biprob01, p01 
	 predict biprob10, p10 
	 predict biprob11, p11 

	* Summarizing predicted values
	 summarize $y1list $y2list biprob1 biprob2 
	 summarize biprob00 biprob01 biprob10 biprob11
	 tabulate $y1list $y2list

	* Efeitos Marginais
	 margins, dydx(*) atmeans predict(p00)
	 margins, dydx(*) atmeans predict(p01)
	 margins, dydx(*) atmeans predict(p10)
	 margins, dydx(*) atmeans predict(p11)

