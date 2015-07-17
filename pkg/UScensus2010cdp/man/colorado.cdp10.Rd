\name{colorado.cdp10}
\alias{colorado.cdp10}
\docType{data}
\title{colorado.cdp10}
\description{
colorado.cdp10 is a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} with polygons made from the 2010 US Census tiger/line boundary files (\url{http://www.census.gov/geo/www/tiger/}) for Census Tracts. It also contains 461 variables from the Summary File 1 (SF 1) which contains the 100-percent data (\url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}). 
 
 All polygons are projected in CRS(" +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
}
\usage{data(colorado.cdp10)}
\details{
\bold{ID Variables} \cr
\tabular{ll}{ 
data field name \tab Full Description \cr
name \tab city name \cr
nameAlevel \tab Name [town/city/etc] (designation)\cr
state \tab State FIPS code \cr
place \tab FIPS code \cr
}

\bold{Census Variables} \cr
\tabular{ll}{ 
Census SF1 Field Name \tab	Full Description \cr
P0010001 \tab TOTAL POPULATION\cr                                                                                                                      
P0020001 \tab URBAN AND RURAL\cr                                                                                                                       
P0020002 \tab Urban:\cr                                                                                                                                
P0020003 \tab Inside urbanized areas\cr                                                                                                                
P0020004 \tab Inside urban clusters\cr                                                                                                                 
P0020005 \tab Rural\cr                                                                                                                                 
P0020006 \tab Not defined for this file\cr                                                                                                             
P0030001 \tab RACE \cr                                                                                                                                 
P0030002 \tab White alone\cr                                                                                                                           
P0030003 \tab Black or African American alone\cr                                                                                                       
P0030004 \tab American Indian and Alaska Native alone\cr                                                                                               
P0030005 \tab Asian alone\cr                                                                                                                           
P0030006 \tab Native Hawaiian and Other Pacific Islander alone\cr                                                                                      
P0030007 \tab Some Other Race alone\cr                                                                                                                 
P0030008 \tab Two or More Races\cr                                                                                                                     
P0040001 \tab HISPANIC OR LATINO ORIGIN\cr                                                                                                             
P0040002 \tab Not Hispanic or Latino\cr                                                                                                                
P0040003 \tab Hispanic or Latino\cr                                                                                                                    
P0050001 \tab HISPANIC OR LATINO ORIGIN BY RACE \cr                                                                                                    
P0050002 \tab Not Hispanic or Latino:\cr                                                                                                               
P0050003 \tab White alone\cr                                                                                                                           
P0050004 \tab Black or African American alone\cr                                                                                                       
P0050005 \tab American Indian and Alaska Native alone\cr                                                                                               
P0050006 \tab Asian alone\cr                                                                                                                           
P0050007 \tab Native Hawaiian and Other Pacific Islander alone\cr                                                                                      
P0050008 \tab Some Other Race alone\cr                                                                                                                 
P0050009 \tab Two or More Races\cr                                                                                                                     
P0050010 \tab Hispanic or Latino:\cr                                                                                                                   
P0050011 \tab White alone\cr                                                                                                                           
P0050012 \tab Black or African American alone\cr                                                                                                       
P0050013 \tab American Indian and Alaska Native alone\cr                                                                                               
P0050014 \tab Asian alone\cr                                                                                                                           
P0050015 \tab Native Hawaiian and Other Pacific Islander alone\cr                                                                                      
P0050016 \tab Some Other Race alone\cr                                                                                                                 
P0050017 \tab Two or More Races\cr                                                                                                                     
P0060001 \tab RACE (TOTAL RACES TALLIED)\cr                                                                                                            
P0060002 \tab White alone or in combination with one or more other races\cr                                                                            
P0060003 \tab Black or African American alone or in combination with one or more other races\cr                                                        
P0060004 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
P0060005 \tab Asian alone or in combination with one or more other races\cr                                                                            
P0060006 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
P0060007 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
P0070001 \tab HISPANIC OR LATINO ORIGIN BY RACE (TOTAL RACES TALLIED)\cr                                                                               
P0070002 \tab Not Hispanic or Latino:\cr                                                                                                               
P0070003 \tab White alone or in combination with one or more other races\cr                                                                            
P0070004 \tab Black or African American alone or in combination with one or more other races\cr                                                        
P0070005 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
P0070006 \tab Asian alone or in combination with one or more other races\cr                                                                            
P0070007 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
P0070008 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
P0070009 \tab Hispanic or Latino:\cr                                                                                                                   
P0070010 \tab White alone or in combination with one or more other races\cr                                                                            
P0070011 \tab Black or African American alone or in combination with one or more other races\cr                                                        
P0070012 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
P0070013 \tab Asian alone or in combination with one or more other races\cr                                                                            
P0070014 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
P0070015 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
P0080001 \tab RACE \cr 
}
\tabular{ll}{                                                                                                                                 
P0080002 \tab Population of one race:\cr                                                                                                               
P0080003 \tab White alone\cr                                                                                                                           
P0080004 \tab Black or African American alone\cr                                                                                                       
P0080005 \tab American Indian and Alaska Native alone\cr                                                                                               
P0080006 \tab Asian alone\cr                                                                                                                           
P0080007 \tab Native Hawaiian and Other Pacific Islander alone\cr                                                                                      
P0080008 \tab Some Other Race alone\cr                                                                                                                 
P0080009 \tab Two or More Races:\cr                                                                                                                    
P0080010 \tab Population of two races:\cr                                                                                                              
P0080011 \tab White; Black or African American\cr                                                                                                      
P0080012 \tab White; American Indian and Alaska Native\cr                                                                                              
P0080013 \tab White; Asian\cr                                                                                                                          
P0080014 \tab White; Native Hawaiian and Other Pacific Islander\cr                                                                                     
P0080015 \tab White; Some Other Race\cr                                                                                                                
P0080016 \tab Black or African American; American Indian and Alaska Native\cr                                                                          
P0080017 \tab Black or African American; Asian\cr                                                                                                      
P0080018 \tab Black or African American; Native Hawaiian and Other Pacific Islander\cr                                                                 
P0080019 \tab Black or African American; Some Other Race\cr                                                                                            
P0080020 \tab American Indian and Alaska Native; Asian\cr                                                                                              
P0080021 \tab American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                                                         
P0080022 \tab American Indian and Alaska Native; Some Other Race\cr                                                                                    
P0080023 \tab Asian; Native Hawaiian and Other Pacific Islander\cr                                                                                     
P0080024 \tab Asian; Some Other Race\cr                                                                                                                
P0080025 \tab Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                           
P0080026 \tab Population of three races:\cr                                                                                                            
P0080027 \tab White; Black or African American; American Indian and Alaska Native\cr                                                                   
P0080028 \tab White; Black or African American; Asian\cr                                                                                               
P0080029 \tab White; Black or African American; Native Hawaiian and Other Pacific Islander\cr                                                          
P0080030 \tab White; Black or African American; Some Other Race\cr                                                                                     
P0080031 \tab White; American Indian and Alaska Native; Asian\cr                                                                                       
P0080032 \tab White; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                                                  
P0080033 \tab White; American Indian and Alaska Native; Some Other Race\cr                                                                             
P0080034 \tab White; Asian; Native Hawaiian and Other Pacific Islander\cr                                                                              
P0080035 \tab White; Asian; Some Other Race\cr                                                                                                         
P0080036 \tab White; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                    
P0080037 \tab Black or African American; American Indian and Alaska Native; Asian\cr                                                                   
P0080038 \tab Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                              
P0080039 \tab Black or African American; American Indian and Alaska Native; Some Other Race\cr                                                         
P0080040 \tab Black or African American; Asian; Native Hawaiian and Other Pacific Islander\cr                                                          
P0080041 \tab Black or African American; Asian; Some Other Race\cr                                                                                     
P0080042 \tab Black or African American; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                
P0080043 \tab American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                                                  
P0080044 \tab American Indian and Alaska Native; Asian; Some Other Race\cr                                                                             
P0080045 \tab American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                        
P0080046 \tab Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                    
P0080047 \tab Population of four races:\cr                                                                                                             
P0080048 \tab White; Black or African American; American Indian and Alaska Native; Asian\cr                                                            
P0080049 \tab White; Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                       
P0080050 \tab White; Black or African American; American Indian and Alaska Native; Some Other Race\cr                                                  
P0080051 \tab White; Black or African American; Asian; Native Hawaiian and Other Pacific Islander\cr                                                   
P0080052 \tab White; Black or African American; Asian; Some Other Race\cr                                                                              
P0080053 \tab White; Black or African American; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                         
P0080054 \tab White; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                                           
P0080055 \tab White; American Indian and Alaska Native; Asian; Some Other Race\cr                                                                      
P0080056 \tab White; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                 
P0080057 \tab White; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                             
P0080058 \tab Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                       
P0080059 \tab Black or African American; American Indian and Alaska Native; Asian; Some Other Race\cr                                                  
P0080060 \tab Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr             
P0080061 \tab Black or African American; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                         
P0080062 \tab American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                 
P0080063 \tab Population of five races:\cr                                                                                                             
P0080064 \tab White; Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                
P0080065 \tab White; Black or African American; American Indian and Alaska Native; Asian; Some Other Race\cr                                           
P0080066 \tab White; Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr      
P0080067 \tab White; Black or African American; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                  
P0080068 \tab White; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                          
P0080069 \tab Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr      
P0080070 \tab Population of six races:\cr                                                                                                              
P0080071 \tab White; Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race \cr
P0090001 \tab HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE\cr                                                                                
P0090002 \tab Hispanic or Latino\cr                                                                                                                    
P0090003 \tab Not Hispanic or Latino:\cr                                                                                                               
P0090004 \tab Population of one race:\cr                                                                                                               
P0090005 \tab White alone\cr                                                                                                                           
P0090006 \tab Black or African American alone\cr                                                                                                       
P0090007 \tab American Indian and Alaska Native alone\cr                                                                                               
P0090008 \tab Asian alone\cr                                                                                                                           
P0090009 \tab Native Hawaiian and Other Pacific Islander alone\cr                                                                                      
P0090010 \tab Some Other Race alone\cr                                                                                                                 
P0090011 \tab Two or More Races:\cr                                                                                                                    
P0090012 \tab Population of two races:\cr                                                                                                              
P0090013 \tab White; Black or African American\cr                                                                                                      
P0090014 \tab White; American Indian and Alaska Native\cr                                                                                              
P0090015 \tab White; Asian\cr                                                                                                                          
P0090016 \tab White; Native Hawaiian and Other Pacific Islander\cr                                                                                     
P0090017 \tab White; Some Other Race\cr                                                                                                                
P0090018 \tab Black or African American; American Indian and Alaska Native\cr                                                                          
P0090019 \tab Black or African American; Asian\cr                                                                                                      
P0090020 \tab Black or African American; Native Hawaiian and Other Pacific Islander\cr                                                                 
P0090021 \tab Black or African American; Some Other Race\cr                                                                                            
P0090022 \tab American Indian and Alaska Native; Asian\cr                                                                                              
P0090023 \tab American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                                                         
P0090024 \tab American Indian and Alaska Native; Some Other Race\cr                                                                                    
P0090025 \tab Asian; Native Hawaiian and Other Pacific Islander\cr                                                                                     
P0090026 \tab Asian; Some Other Race\cr                                                                                                                
P0090027 \tab Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                           
P0090028 \tab Population of three races:\cr                                                                                                            
P0090029 \tab White; Black or African American; American Indian and Alaska Native\cr                                                                   
P0090030 \tab White; Black or African American; Asian\cr                                                                                               
P0090031 \tab White; Black or African American; Native Hawaiian and Other Pacific Islander\cr                                                          
P0090032 \tab White; Black or African American; Some Other Race\cr                                                                                     
P0090033 \tab White; American Indian and Alaska Native; Asian\cr                                                                                       
P0090034 \tab White; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                                                  
P0090035 \tab White; American Indian and Alaska Native; Some Other Race\cr                                                                             
P0090036 \tab White; Asian; Native Hawaiian and Other Pacific Islander\cr                                                                              
P0090037 \tab White; Asian; Some Other Race\cr                                                                                                         
P0090038 \tab White; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                    
P0090039 \tab Black or African American; American Indian and Alaska Native; Asian\cr                                                                   
P0090040 \tab Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                              
P0090041 \tab Black or African American; American Indian and Alaska Native; Some Other Race\cr                                                         
P0090042 \tab Black or African American; Asian; Native Hawaiian and Other Pacific Islander\cr                                                          
P0090043 \tab Black or African American; Asian; Some Other Race\cr                                                                                     
P0090044 \tab Black or African American; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                
P0090045 \tab American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                                                  
P0090046 \tab American Indian and Alaska Native; Asian; Some Other Race\cr                                                                             
P0090047 \tab American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                        
P0090048 \tab Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                                    
P0090049 \tab Population of four races:\cr                                                                                                             
P0090050 \tab White; Black or African American; American Indian and Alaska Native; Asian\cr                                                            
P0090051 \tab White; Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander\cr                       
P0090052 \tab White; Black or African American; American Indian and Alaska Native; Some Other Race\cr                                                  
P0090053 \tab White; Black or African American; Asian; Native Hawaiian and Other Pacific Islander\cr                                                   
P0090054 \tab White; Black or African American; Asian; Some Other Race\cr                                                                              
P0090055 \tab White; Black or African American; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                         
P0090056 \tab White; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                                           
P0090057 \tab White; American Indian and Alaska Native; Asian; Some Other Race\cr                                                                      
P0090058 \tab White; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                 
P0090059 \tab White; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                                             
P0090060 \tab Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                       
P0090061 \tab Black or African American; American Indian and Alaska Native; Asian; Some Other Race\cr                                                  
P0090062 \tab Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr             
P0090063 \tab Black or African American; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                         
P0090064 \tab American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                 
P0090065 \tab Population of five races:\cr                                                                                                             
P0090066 \tab White; Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander\cr                
P0090067 \tab White; Black or African American; American Indian and Alaska Native; Asian; Some Other Race\cr                                           
P0090068 \tab White; Black or African American; American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander; Some Other Race\cr      
P0090069 \tab White; Black or African American; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                                  
P0090070 \tab White; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr                          
P0090071 \tab Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race\cr      
P0090072 \tab Population of six races:\cr                                                                                                              
P0090073 \tab White; Black or African American; American Indian and Alaska Native; Asian; Native Hawaiian and Other Pacific Islander; Some Other Race \cr
}
\tabular{ll}{ 
H0010001 \tab HOUSING UNITS \cr                                                                                                                        
H0020001 \tab URBAN AND RURAL\cr                                                                                                                       
H0020002 \tab Urban:\cr                                                                                                                                
H0020003 \tab Inside urbanized areas\cr                                                                                                                
H0020004 \tab Inside urban clusters\cr                                                                                                                 
H0020005 \tab Rural\cr                                                                                                                                 
H0020006 \tab Not defined for this file\cr                                                                                                             
H0030001 \tab OCCUPANCY STATUS\cr                                                                                                                      
H0030002 \tab Occupied\cr                                                                                                                              
H0030003 \tab Vacant\cr                                                                                                                                
H0040001 \tab TENURE\cr                                                                                                                                
H0040002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H0040003 \tab Owned free and clear\cr                                                                                                                  
H0040004 \tab Renter occupied\cr                                                                                                                       
H0050001 \tab VACANCY STATUS \cr                                                                                                                       
H0050002 \tab For rent\cr                                                                                                                              
H0050003 \tab Rented, not occupied\cr                                                                                                                  
H0050004 \tab For sale only\cr                                                                                                                         
H0050005 \tab Sold, not occupied\cr                                                                                                                    
H0050006 \tab For seasonal, recreational, or occasional use\cr                                                                                         
H0050007 \tab For migrant workers\cr                                                                                                                   
H0050008 \tab Other vacant\cr                                                                                                                          
H0060001 \tab RACE OF HHER\cr                                                                                                                          
H0060002 \tab Householder who is White alone\cr                                                                                                        
H0060003 \tab Householder who is Black or African American alone\cr                                                                                    
H0060004 \tab Householder who is American Indian and Alaska Native alone\cr                                                                            
H0060005 \tab Householder who is Asian alone\cr                                                                                                        
H0060006 \tab Householder who is Native Hawaiian and Other Pacific Islander alone\cr                                                                   
H0060007 \tab Householder who is Some Other Race alone\cr                                                                                              
H0060008 \tab Householder who is Two or More Races\cr                                                                                                  
H0070001 \tab HISPANIC OR LATINO ORIGIN OF HHER BY RACE OF HHER \cr                                                                                    
H0070002 \tab Not Hispanic or Latino householder:\cr                                                                                                   
H0070003 \tab Householder who is White alone\cr                                                                                                        
H0070004 \tab Householder who is Black or African American alone\cr                                                                                    
H0070005 \tab Householder who is American Indian and Alaska Native alone\cr                                                                            
H0070006 \tab Householder who is Asian alone\cr                                                                                                        
H0070007 \tab Householder who is Native Hawaiian and Other Pacific Islander alone\cr                                                                   
H0070008 \tab Householder who is Some Other Race alone\cr                                                                                              
H0070009 \tab Householder who is Two or More Races\cr                                                                                                  
H0070010 \tab Hispanic or Latino householder:\cr                                                                                                       
H0070011 \tab Householder who is White alone\cr                                                                                                        
H0070012 \tab Householder who is Black or African American alone\cr                                                                                    
H0070013 \tab Householder who is American Indian and Alaska Native alone\cr                                                                            
H0070014 \tab Householder who is Asian alone\cr                                                                                                        
H0070015 \tab Householder who is Native Hawaiian and Other Pacific Islander alone\cr                                                                   
H0070016 \tab Householder who is Some Other Race alone\cr                                                                                              
H0070017 \tab Householder who is Two or More Races\cr 
}
\tabular{ll}{                                                                                                  
H0080001 \tab TOTAL RACES TALLIED FOR HHERS \cr                                                                                                        
H0080002 \tab White alone or in combination with one or more other races\cr                                                                            
H0080003 \tab Black or African American alone or in combination with one or more other races\cr                                                        
H0080004 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
H0080005 \tab Asian alone or in combination with one or more other races\cr                                                                            
H0080006 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
H0080007 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
H0090001 \tab HISPANIC OR LATINO ORIGIN OF HHERS BY TOTAL RACES TALLIED \cr                                                                            
H0090002 \tab Total races tallied for Not Hispanic or Latino householders:\cr                                                                          
H0090003 \tab White alone or in combination with one or more other races\cr                                                                            
H0090004 \tab Black or African American alone or in combination with one or more other races\cr                                                        
H0090005 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
H0090006 \tab Asian alone or in combination with one or more other races\cr                                                                            
H0090007 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
H0090008 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
H0090009 \tab Total races tallied for Hispanic or Latino householders:\cr                                                                              
H0090010 \tab White alone or in combination with one or more other races\cr                                                                            
H0090011 \tab Black or African American alone or in combination with one or more other races\cr                                                        
H0090012 \tab American Indian and Alaska Native alone or in combination with one or more other races\cr                                                
H0090013 \tab Asian alone or in combination with one or more other races\cr                                                                            
H0090014 \tab Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races\cr                                       
H0090015 \tab Some Other Race alone or in combination with one or more other races\cr                                                                  
H0100001 \tab TOTAL POPULATION IN OCCUPIED HOUSING UNITS \cr      
H0110001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE \cr                                                                                
H0110002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H0110003 \tab Owned free and clear\cr                                                                                                                  
H0110004 \tab Renter occupied\cr                                                                                                                       
H0120001 \tab AVE. HH SIZE OF OCCUPIED HOUSING UNITS BY TENURE (2 EXPRESSED DECIMALS)\cr                                                               
H0120002 \tab Owner occupied\cr                                                                                                                        
H0120003 \tab Renter occupied\cr                                                                                                                       
H0130001 \tab HH SIZE \cr                                                                                                                              
H0130002 \tab 1-person household\cr                                                                                                                    
H0130003 \tab 2-person household\cr                                                                                                                    
H0130004 \tab 3-person household\cr                                                                                                                    
H0130005 \tab 4-person household\cr                                                                                                                    
H0130006 \tab 5-person household\cr                                                                                                                    
H0130007 \tab 6-person household\cr                                                                                                                    
H0130008 \tab 7-or-more-person household\cr                                                                                                            
H0140001 \tab TENURE BY RACE OF HHER\cr                                                                                                                
H0140002 \tab Owner occupied:\cr                                                                                                                       
H0140003 \tab Householder who is White alone\cr                                                                                                        
H0140004 \tab Householder who is Black or African American alone\cr                                                                                    
H0140005 \tab Householder who is American Indian and Alaska Native alone\cr                                                                            
H0140006 \tab Householder who is Asian alone\cr                                                                                                        
H0140007 \tab Householder who is Native Hawaiian and Other Pacific Islander alone\cr                                                                   
H0140008 \tab Householder who is Some Other Race alone\cr                                                                                              
H0140009 \tab Householder who is Two or More Races\cr                                                                                                  
H0140010 \tab Renter occupied:\cr                                                                                                                      
H0140011 \tab Householder who is White alone\cr                                                                                                        
H0140012 \tab Householder who is Black or African American alone\cr                                                                                    
H0140013 \tab Householder who is American Indian and Alaska Native alone\cr                                                                            
H0140014 \tab Householder who is Asian alone\cr                                                                                                        
H0140015 \tab Householder who is Native Hawaiian and Other Pacific Islander alone\cr                                                                   
H0140016 \tab Householder who is Some Other Race alone\cr                                                                                              
H0140017 \tab Householder who is Two or More Races\cr                                                                                                  
H0150001 \tab TENURE BY HISPANIC OR LATINO ORIGIN OF HHER \cr                                                                                          
H0150002 \tab Owner occupied:\cr                                                                                                                       
H0150003 \tab Not Hispanic or Latino householder\cr                                                                                                    
H0150004 \tab Hispanic or Latino householder\cr                                                                                                        
H0150005 \tab Renter occupied:\cr                                                                                                                      
H0150006 \tab Not Hispanic or Latino householder\cr                                                                                                    
H0150007 \tab Hispanic or Latino householder\cr                                                                                                        
H0160001 \tab TENURE BY HH SIZE \cr                                                                                                                    
H0160002 \tab Owner occupied:\cr                                                                                                                       
H0160003 \tab 1-person household\cr                                                                                                                    
H0160004 \tab 2-person household\cr                                                                                                                    
H0160005 \tab 3-person household\cr                                                                                                                    
H0160006 \tab 4-person household\cr                                                                                                                    
H0160007 \tab 5-person household\cr                                                                                                                    
H0160008 \tab 6-person household\cr                                                                                                                    
H0160009 \tab 7-or-more-person household\cr                                                                                                            
H0160010 \tab Renter occupied:\cr                                                                                                                      
H0160011 \tab 1-person household\cr                                                                                                                    
H0160012 \tab 2-person household\cr                                                                                                                    
H0160013 \tab 3-person household\cr                                                                                                                    
H0160014 \tab 4-person household\cr                                                                                                                    
H0160015 \tab 5-person household\cr                                                                                                                    
H0160016 \tab 6-person household\cr                                                                                                                    
H0160017 \tab 7-or-more-person household\cr                                                                                                            
H0170001 \tab TENURE BY AGE OF HHER\cr                                                                                                                 
H0170002 \tab Owner occupied:\cr                                                                                                                       
H0170003 \tab Householder 15 to 24 years\cr                                                                                                            
H0170004 \tab Householder 25 to 34 years\cr                                                                                                            
H0170005 \tab Householder 35 to 44 years\cr                                                                                                            
H0170006 \tab Householder 45 to 54 years\cr                                                                                                            
H0170007 \tab Householder 55 to 59 years\cr                                                                                                            
H0170008 \tab Householder 60 to 64 years\cr                                                                                                            
H0170009 \tab Householder 65 to 74 years\cr                                                                                                            
H0170010 \tab Householder 75 to 84 years\cr                                                                                                            
H0170011 \tab Householder 85 years and over\cr                                                                                                         
H0170012 \tab Renter occupied:\cr                                                                                                                      
H0170013 \tab Householder 15 to 24 years\cr                                                                                                            
H0170014 \tab Householder 25 to 34 years\cr                                                                                                            
H0170015 \tab Householder 35 to 44 years\cr                                                                                                            
H0170016 \tab Householder 45 to 54 years\cr                                                                                                            
H0170017 \tab Householder 55 to 59 years\cr                                                                                                            
H0170018 \tab Householder 60 to 64 years\cr                                                                                                            
H0170019 \tab Householder 65 to 74 years\cr                                                                                                            
H0170020 \tab Householder 75 to 84 years\cr                                                                                                            
H0170021 \tab Householder 85 years and over\cr                                                                                                         
H0180001 \tab TENURE BY HH TYPE  BY AGE OF HHER\cr
H0180002 \tab Owner occupied:\cr                                                                                                                       
H0180003 \tab Family households:\cr                                                                                                                    
H0180004 \tab Husband-wife family:\cr                                                                                                                  
H0180005 \tab Householder 15 to 34 years\cr                                                                                                            
H0180006 \tab Householder 35 to 64 years\cr                                                                                                            
H0180007 \tab Householder 65 years and over\cr                                                                                                         
H0180008 \tab Other family:\cr                                                                                                                         
H0180009 \tab Male householder, no wife present:\cr                                                                                                    
H0180010 \tab Householder 15 to 34 years\cr                                                                                                            
H0180011 \tab Householder 35 to 64 years\cr                                                                                                            
H0180012 \tab Householder 65 years and over\cr                                                                                                         
H0180013 \tab Female householder, no husband present:\cr                                                                                               
H0180014 \tab Householder 15 to 34 years\cr                                                                                                            
H0180015 \tab Householder 35 to 64 years\cr                                                                                                            
H0180016 \tab Householder 65 years and over\cr                                                                                                         
H0180017 \tab Nonfamily households:\cr                                                                                                                 
H0180018 \tab Male householder:\cr                                                                                                                     
H0180019 \tab Living alone:\cr                                                                                                                         
H0180020 \tab Householder 15 to 34 years\cr                                                                                                            
H0180021 \tab Householder 35 to 64 years\cr                                                                                                            
H0180022 \tab Householder 65 years and over\cr                                                                                                         
H0180023 \tab Not living alone:\cr                                                                                                                     
H0180024 \tab Householder 15 to 34 years\cr                                                                                                            
H0180025 \tab Householder 35 to 64 years\cr                                                                                                            
H0180026 \tab Householder 65 years and over\cr                                                                                                         
H0180027 \tab Female householder:\cr                                                                                                                   
H0180028 \tab Living alone:\cr                                                                                                                         
H0180029 \tab Householder 15 to 34 years\cr                                                                                                            
H0180030 \tab Householder 35 to 64 years\cr                                                                                                            
H0180031 \tab Householder 65 years and over\cr                                                                                                         
H0180032 \tab Not living alone:\cr                                                                                                                     
H0180033 \tab Householder 15 to 34 years\cr                                                                                                            
H0180034 \tab Householder 35 to 64 years\cr                                                                                                            
H0180035 \tab Householder 65 years and over\cr                                                                                                         
H0180036 \tab Renter occupied:\cr                                                                                                                      
H0180037 \tab Family households:\cr                                                                                                                    
H0180038 \tab Husband-wife family:\cr                                                                                                                  
H0180039 \tab Householder 15 to 34 years\cr                                                                                                            
H0180040 \tab Householder 35 to 64 years\cr                                                                                                            
H0180041 \tab Householder 65 years and over\cr                                                                                                         
H0180042 \tab Other family:\cr                                                                                                                         
H0180043 \tab Male householder, no wife present:\cr                                                                                                    
H0180044 \tab Householder 15 to 34 years\cr                                                                                                            
H0180045 \tab Householder 35 to 64 years\cr                                                                                                            
H0180046 \tab Householder 65 years and over\cr                                                                                                         
H0180047 \tab Female householder, no husband present:\cr                                                                                               
H0180048 \tab Householder 15 to 34 years\cr                                                                                                            
H0180049 \tab Householder 35 to 64 years\cr                                                                                                            
H0180050 \tab Householder 65 years and over\cr                                                                                                         
H0180051 \tab Nonfamily households:\cr                                                                                                                 
H0180052 \tab Male householder:\cr                                                                                                                     
H0180053 \tab Living alone:\cr                                                                                                                         
H0180054 \tab Householder 15 to 34 years\cr                                                                                                            
H0180055 \tab Householder 35 to 64 years\cr                                                                                                            
H0180056 \tab Householder 65 years and over\cr                                                                                                         
H0180057 \tab Not living alone:\cr                                                                                                                     
H0180058 \tab Householder 15 to 34 years\cr                                                                                                            
H0180059 \tab Householder 35 to 64 years\cr                                                                                                            
H0180060 \tab Householder 65 years and over\cr                                                                                                         
H0180061 \tab Female householder:\cr                                                                                                                   
H0180062 \tab Living alone:\cr                                                                                                                         
H0180063 \tab Householder 15 to 34 years\cr                                                                                                            
H0180064 \tab Householder 35 to 64 years\cr                                                                                                            
H0180065 \tab Householder 65 years and over\cr                                                                                                         
H0180066 \tab Not living alone:\cr                                                                                                                     
H0180067 \tab Householder 15 to 34 years\cr                                                                                                            
H0180068 \tab Householder 35 to 64 years\cr                                                                                                            
H0180069 \tab Householder 65 years and over\cr                                                                                                         
H0190001 \tab TENURE BY PRESENCE OF PEOPLE UNDER 18 YEARS (EXCLUDING HHERS, SPOUSES, AND UNMARRIED PARTNERS) \cr
}
\tabular{ll}{ 
H0190002 \tab Owner-occupied:\cr                                                                                                                       
H0190003 \tab With children under 18 years\cr                                                                                                          
H0190004 \tab No children under 18 years\cr                                                                                                            
H0190005 \tab Renter-occupied:\cr                                                                                                                      
H0190006 \tab With children under 18 years\cr                                                                                                          
H0190007 \tab No children under 18 years\cr                                                                                                            
H0200001 \tab OCCUPIED HOUSING UNITS SUBSTITUTED \cr                                                                                                   
H0200002 \tab Substituted\cr                                                                                                                           
H0200003 \tab Not substituted\cr                                                                                                                       
H0210001 \tab ALLOCATION OF VACANCY STATUS\cr                                                                                                          
H0210002 \tab Allocated\cr                                                                                                                             
H0210003 \tab Not allocated\cr                                                                                                                         
H0220001 \tab ALLOCATION OF TENURE \cr                                                                                                                 
H0220002 \tab Allocated\cr                                                                                                                             
H0220003 \tab Not allocated\cr                                                                                                                         
H011A001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (WHITE ALONE HHER)\cr                                                              
H011A002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011A003 \tab Owned free and clear\cr                                                                                                                  
H011A004 \tab Renter occupied\cr                                                                                                                       
H011B001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (BLACK OR AFRICAN AMERICAN ALONE HHER)\cr                                          
H011B002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011B003 \tab Owned free and clear\cr                                                                                                                  
H011B004 \tab Renter occupied\cr                                                                                                                       
H011C001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HHER)\cr                                  
H011C002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011C003 \tab Owned free and clear\cr                                                                                                                  
H011C004 \tab Renter occupied\cr                                                                                                                       
H011D001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (ASIAN ALONE HHER)\cr                                                              
H011D002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011D003 \tab Owned free and clear\cr                                                                                                                  
H011D004 \tab Renter occupied\cr                                                                                                                       
H011E001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HHER)\cr                         
H011E002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011E003 \tab Owned free and clear\cr                                                                                                                  
H011E004 \tab Renter occupied\cr                                                                                                                       
H011F001 \tab TOTAL  POPULATION IN OCCUPIED HOUSING UNITS BY TENURE (SOME OTHER RACE ALONE HHER)\cr                                                    
H011F002 \tab Owned with a mortgage or a loan\cr                                                                                                       
H011F003 \tab Owned free and clear\cr                                                                                                                  
H011F004 \tab Renter occupied\cr          
}


}
\source{
Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001.
}
\references{

Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}.\cr
\url{http://www.census.gov/ }\cr
\url{http://www2.census.gov/cgi-bin/shapefiles/national-files} \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf} \cr

}
\examples{
data(colorado.cdp10)
plot(colorado.cdp10)
}
\keyword{datasets}
