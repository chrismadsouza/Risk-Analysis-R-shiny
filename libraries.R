library(shiny)
library(shinydashboard)
library(plotly)
library(Quandl)
library(cvar)
library(sentimentr)
library(devtools)
install_github("nik01010/dashboardthemes",force=TRUE)
library(dashboardthemes)
#install below packages if it is missing in above packages
#library(digest)
# library(Rcpp)

Quandl.api_key('uR9WuvxNu3igGVwWeEdy')          #Unique Quandl API key

#Loading datasets from Quandl with Ticker Symbol
#NSE
Bajaj=Quandl('XNSE/BAJAJ_AUTO')
BASF=Quandl('XNSE/BASF')
ACC=Quandl('XNSE/ACC')
Biocon=Quandl('XNSE/BIOCON')
BHEL=Quandl('XNSE/BHEL')

#NASDAQ
apple=Quandl('XNAS/AAPL')
atlantic=Quandl('XNAS/AAME')
airlines=Quandl('XNAS/AAL')
ACI=Quandl('XNAS/ACIW')
Abaxis=Quandl('XNAS/ABAX')

#NYS
ABB=Quandl('XNYS/ABB')
alliance=Quandl('XNYS/AB')
Abbott=Quandl('XNYS/ABT')
Archer=Quandl('XNYS/ADM')
American=Quandl('XNYS/AAT')


#Calculating returns for each dataset by defining a function called returns
returns <- function(data)
{
  data$'Adjustment Factor' <- NULL    #Dropping empty columns adjustment Factor and adjustment Type
  data$'Adjustment Type' <- NULL
  return=((data$Close/data$Open)-1)*100   #Calculating Return value
  data["Returns"] <- I(return)     #adding a column called Returns which contains daily return value 
  data=as.data.frame(data)
  
}

#Finding returns for all Datasets
Bajaj= returns(Bajaj)
BASF= returns(BASF)
ACC= returns(ACC)
Biocon= returns(Biocon)
BHEL= returns(BHEL)
apple=apple[-c(445),]     #Removing this particular value as it is an outlier
apple= returns(apple)
atlantic= returns(atlantic)
airlines= returns(airlines)
ACI= returns(ACI)
Abaxis= returns(Abaxis)
ABB= returns(ABB)
alliance= returns(alliance)
Abbott=returns(Abbott)
Archer= returns(Archer)
American= returns(American)

#Putting all datasets in a datafile which is a list
datafiles<-list(Bajaj,BASF,ACC,
                Biocon,
                BHEL,
                apple,
                atlantic,
                airlines,
                ACI,
                Abaxis,
                ABB,
                alliance,
                Abbott,
                Archer,
                American)

### creating custom theme object for dashboard
theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 16
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


#Below code represents how sentiment value was calculated for each of the 6 articles 
# 
# #Sentiment Analysis
# #1
 sentiment_by("Biotechnology major Biocon Friday said the European Commission has granted marketing authorisation to its partner Mylan for Fulphila, a biosimilar product to treat cancer.
 
              Fulphila, a biosimilar Pegfilgrastim, jointly developed by Biocon and Mylan, has been approved in European Union (EU), Biocon said in a regulatory filing.
                            In September, European Medicines Agency's Committee for Medicinal Products for Human Use had issued a positive opinion recommending approval of the product which is biosimilar to Amgen's Neulasta.
              
              It is indicated for the reduction in the duration of neutropenia and the incidence of febrile neutropenia in adult patients treated with cytotoxic chemotherapy for malignancy.#

              
              According to IQVIA, Neulasta had sales of more than USD 450 million (Rs 3136 crore) in Europe for the 12 months ending June 30, 2018.
              
              Biocon shares Friday ended 3.27 percent up at Rs 622.25 apiece on the BSE.")
# 
# sentiment("Sandoz, a Novartis division and the global leader in biosimilars, today announced a global partnership with Asia's premier biopharmaceuticals company, Biocon, to develop, manufacture and commercialize multiple biosimilars in immunology and oncology for patients worldwide. 
# Under the terms of the agreement, both companies will share responsibility for end-to-end development, manufacturing and global regulatory approvals for a number of products, and will have a cost and profit share arrangement globally. Worldwide commercialization responsibilities will be divided and each company's strengths will be leveraged within specific geographies. Sandoz will lead commercialization in North America* and the EU,** while Biocon will lead commercialization in Rest of the World.*** 
#           "Today's announcement bolsters our leadership position in biosimilars and positions us to continue to lead well into the future," said Richard Francis, CEO, Sandoz. "Biocon is a great complement to our proven biosimilar capabilities at Sandoz. Through this collaboration, we are reinforcing our long-term commitment to increase patient access to biologics." 
#           "Together, we will be able to realize benefits at every stage of the value chain, from development, through manufacturing to commercialization," said Carol Lynch, Global Head, Biopharmaceuticals, Sandoz. "This collaboration further strengthens our ability to deliver next-generation biosimilar medicines to patients."
#           Sandoz is committed to increasing patient access to high-quality biosimilars. We are the global leader in biosimilars, with five biosimilars currently marketed worldwide, as well as a leading global pipeline. Sandoz is well-positioned to continue leading the biosimilars industry based on our experience and capabilities in development, manufacturing and commercialization. As a division of Novartis, the first global healthcare company to establish a leading position in both innovative and off-patent medicines, we benefit strongly from this unique blend of experience and expertise in many different market environments.
#           As an innovation-led biopharmaceutical company, Biocon has successfully developed and taken a range of novel biologics, biosimilar antibodies, rh-insulin and insulin analogs from 'lab to market'. The collaboration with Sandoz builds upon Biocon's successful progress in its existing global biosimilars program. An early mover in the biosimilars space, Biocon has successfully launched its insulin glargine in Japan, trastuzumab and bevacizumab biosimilars in India and rh-insulin, insulin glargine and biosimilar trastuzumab in a few emerging markets; and it was the first Indian company to have a biosimilar approved by the US Food and Drug Administration. 
#           Disclaimer
#           
#           This press release contains forward-looking statements within the meaning of the United States Private Securities Litigation Reform Act of 1995. Forward-looking statements can generally be identified by words such as "to develop," "to commercialize," "potential," "can," "will," "plan," "expect," "anticipate," "look forward," "believe," "committed," "investigational," "pipeline," "launch," "expansion," "portfolio," "collaboration," "partnership," or similar terms, or by express or implied discussions regarding potential marketing approvals, new indications or labeling for the investigational or approved biosimilar products described in this press release, or regarding potential future revenues from such products or the collaboration and partnership with Biocon. You should not place undue reliance on these statements. Such forward-looking statements are based on our current beliefs and expectations regarding future events, and are subject to significant known and unknown risks and uncertainties. Should one or more of these risks or uncertainties materialize, or should underlying assumptions prove incorrect, actual results may vary materially from those set forth in the forward-looking statements. There can be no guarantee that the investigational or approved products described in this press release will be submitted or approved for sale or for any additional indications or labeling in any market, or at any particular time. Neither can there be any guarantee that, if approved, such biosimilar products will be approved for all indications included in the reference product's label. Nor can there be any guarantee that such products will be commercially successful in the future. Neither can there be any guarantee that the collaboration and partnership with Biocon will achieve any or all of its intended goals and objectives, or be commercially successful. In particular, our expectations regarding such products, and the collaboration and partnership with Biocon, could be affected by, among other things, the uncertainties inherent in research and development, including clinical trial results and additional analysis of existing clinical data; regulatory actions or delays or government regulation generally; the particular prescribing preferences of physicians and patients; competition in general, including potential approval of additional biosimilar versions of such products; global trends toward health care cost containment, including government, payor and general public pricing and reimbursement pressures; litigation outcomes, including intellectual property disputes or other legal efforts to prevent or limit Sandoz or Biocon from selling the products developed, manufactured and commercialized under the collaboration and partnership; general economic and industry conditions, including the effects of the persistently weak economic and financial environment in many countries; potential or actual data security and data privacy issues; safety, quality or manufacturing issues, and other risks and factors referred to in Novartis AG's current Form 20-F on file with the US Securities and Exchange Commission. Novartis is providing the information in this press release as of this date and does not undertake any obligation to update any forward-looking statements contained in this press release as a result of new information, future events or otherwise. 
#           About Sandoz 
#           
#           Sandoz is a global leader in generic pharmaceuticals and biosimilars. As a division of the Novartis Group, our purpose is to discover new ways to improve and extend people's lives. We contribute to society's ability to support growing healthcare needs by pioneering novel approaches to help people around the world access high-quality medicine. Our portfolio of approximately 1000 molecules, covering all major therapeutic areas, accounted for 2016 sales of USD 10.1 billion. In 2016, our products reached well over 500 million patients and we aspire to reach one billion. Sandoz is headquartered in Holzkirchen, in Germany's Greater Munich area.")
# 
# sentiment_by("Biotech major Biocon will invest ???1,060 crore to set up a manufacturing plant near Bengaluru.
# At a meeting held earlier this week, headed by Karnataka Chief Minister Siddaramaiah, the State government cleared the company's proposal to set up a plant to manufacture injectables, monoclonal antibodies and oral solid dosage (OSD) forms as part of its biosimilars, generics formulation and novel molecules businesses. Monoclonal antibodies are specific antibodies that are made in a lab to mimic the body's immune system.
#           Job creation 
#           The plant is expected to create 750 jobs, said Biocon.
#           In a recent call, Biocon Chairperson and MD Kiran Mazumdar-Shaw had said that clinical development of the company's novel anti-CD6 monoclonal antibody Itolizumab is taking place in Australia, with healthy volunteers. "These trials will evaluate pharmacokinetics and establish comparability of the subcutaneous route of administration of Itolizumab in comparison to the intravenous route currently approved in India," she added.
#           Pharmacokinetics is the study of the movement of drugs in the human body.
#           The Karnataka State High Level Committee Meeting also cleared Pepsico's plan to set up a beverage and snack manufacturing unit at an investment of ???590 crore at Nanjangud (Mysuru district).
#           That project is likely to create 900 jobs.")
# sentiment_by("Biopharmaceutical firm Biocon and its partner Mylan on September 21 said the European Medicines Agency's Committee for Medicinal Products for Human Use (CHMP) has recommended approval of Filphila or pegfilgrastim.
# Pegfilgrastim is a biosimilar version of Amgen's Neulasta used to stimulate the level of neutrophils, a type of white blood cell that gets damaged in cancer patients undergoing chemotherapy.
# The CHMP recommendation takes Biocon a step closer towards the approval of biosimilar pegfilgrastim in Europe.
# Amgen's Neulasta had brand sales of more than $450 million in Europe for the 12 months ending June 30, 2018, according to market research firm IQVIA.
# 
# 
# related news
# 
# Northern Arc Capital ropes in UTI MF's Leo Puri as Chairman
# 
# 
# General insurers' premium rises 23% to Rs 12,959 crore in February: IRDAI
# 
# 
# Hotel Leela Venture to sell hotels, property to Brookfield for Rs 3,950 crore
# 
# "The CHMP positive opinion is based upon a review of evidence demonstrating biosimilarity," Biocon said in a statement.
# The CHMP opinion will now be considered by the European Commission for approval.
# The decision on approval is expected by November 2018, the company said.
# "Data submitted as part of the Marketing Authorization Application included similarity assessment in analytical testing, preclinical and clinical studies that demonstrated biosimilarity to the reference product, Neulasta," Biocon said.
# Fulphila was approved by US FDA earlier this year and is the first USFDA approved biosimilar for Neulasta in the US.
# Regulatory applications for Fulphila also have been submitted in Australia, New Zealand, Canada and several other countries.
# CHMP's decision to recommend approval of Biocon and Mylan's biosimilar Pegfilgrastim brings us a step closer to offer this high quality, affordable biologic therapy for cancer patients in the EU, having launched this product in the US, earlier this year," said Biocon CEO and Joint Managing Director Arun Chandavarkar.
#              Mylan and Biocon are exclusive partners on a broad portfolio of biosimilar and insulin products.
#              Neulasta is one of 11 biologic and insulin products co-developed by Mylan and Biocon for the global marketplace.
#              Mylan has exclusive commercialization rights for the product in the US, Canada, Japan, Australia, New Zealand and in the European Union and European Free Trade Association countries. Biocon has co-exclusive commercialisation rights with Mylan for the product in the rest of the world.
#              While Biocon develops and manufactures the drug, Mylan takes care of regulatory approvals and marketing. ")
# sentiment_by("Kiran Mazumdar-led Biocon has on Thursday (17 March) signed a generic recombinant human insulin (rh-insulin) development and marketing agreement with Mexico's Laboratorios PiSA SA for the US market. This collaboration is a part of Biocon's strategy to address the large demand for generic rh-insulin in the US, which accounts for over 40 per cent of the global sales of $5 billion.
# 
#              It also marks an extension of Biocon's 10-years long relationship with the Mexican company, which is a dominant player in insulins in that market. Biocon's Insulin Glargine was the first to be approved in Mexico in 2015. with the ne partnership, both companies are committed to providing affordable access to insulins to patients. This is a cost and profit sharing agreement in which Biocon will be responsible for clinical development, regulatory approvals, and commercialization of the product in the US. 
#              
#              This partnership will leverage Biocon's manufacturing facilities for the drug substance and Laboratorios' drug product facilities in Mexico. In addition, this arrangement will take advantage of PiSA's proximity to the US market and Mexico's NAFTA membership, which will ensure an efficient and optimal supply chain to address the needs of the US healthcare system for the new product. 
#              
#              Biocon's global clinical development experience with Insulin Glargine for the US will be a useful precedent in developing rh-insulin for the US market. Through this collaboration, the Indian company we will introduce rh- insulin under the Biocon brand to address the $2 billion market opportunity in the US. 
#              
#              Our partnership with PiSA demonstrates our commitment to provide access to affordable insulins to patients in the US, said 
#              
#              Biocon Chairperson & Managing Director Kiran Mazumdar-Shaw. 
#              
#              This collaboration will enable us to manufacture the rh-insulin drug product at PiSA's facilities in Mexico and commercialize it under Brand Biocon in the US market, which has a huge diabetes burden with over 1.4 million people diagnosed with diabetes every year, she added.
#              
#              Biocon is currently one of the large insulins producers in Asia and it has a range of products including rh-Insulin (Insugen) and Insulin Glargine (Basalog) in India and several emerging markets. It also has marketing approvals in at least 60 countries for rh-Insulin and in some 20 countries for Insulin Glargine.")
# 
# sentiment("Biocon said it was "incorrect to say its Enbrel programme has failed" and the reason for the slow development was because the company chose to focus on other drugs in its pipeline. ")
# 
# 
# 
# 