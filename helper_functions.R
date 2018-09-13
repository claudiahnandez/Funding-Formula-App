library(RODBC)
library(stringr)
library(plyr)
library(dplyr)

uid = "ommited to post on github"
pwd = "ommited to post on github"
MAX_YEARS_TO_SHOW<-5

get_ftes<- function(semester,max_academic_year, department, discipline){
   max_academic_year=gsub("-", "", max_academic_year)#Academic year is in format year-year not yearyear 
   min_academic_year<-get_min_years(max_academic_year)

  query = paste0(
   "select Academic_Year,")
  
  if(!is.null(department) && department!="All Departments")
    query<-paste0(query,"Department, ")
  
  if(!is.null(discipline) && discipline!="All Disciplines")
    query<-paste0(query,"Discipline, ")
 
    query<-paste0(query,
   "sum(FTES)as FTES,
    sum (case when Noncredit=0 then FTES else 0 end ) AS Credit,
    sum(case when Noncredit=1 then FTES else 0 end ) AS Noncredit,
    sum(case when Program='Adult Education' then FTES else 0 end ) AS AdultEd,
    sum(case when Program='Dual Enrollment' then FTES else 0 end ) AS Concurrent
    from sections2
    where College='E' ")
      
  if(!is.null(department) && department!="All Departments")
      query<-paste0(query," and Department='",department,"' ")
  
  if(!is.null(discipline) && discipline!="All Disciplines"){
    query<-paste0(query," and Discipline='",discipline,"' ")
  }
  if(!is.null(semester)){
    query<-paste0(query,"and Semester=",sem_to_num(semester))
  }
  query<-paste0(query,
    " and Status='Open' and Accounting_Method!='Not FTES generating' and Academic_Year<='",max_academic_year,"'
     and Academic_Year>'",min_academic_year,"'
    Group By Academic_Year ")
 
  if(!is.null(department) && department!="All Departments")
    query<-paste0(query,",Department ")
  

  if(!is.null(discipline) && discipline!="All Disciplines"){
    query<-paste0(query,",Discipline ")
  }

  query<-paste0(query,"order by Academic_Year")

  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  data <- sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  #write(query,"ftes_query.txt")
  
  #fix academic_year
  if(!is.null(data))
      data$Academic_Year= paste0(substr(data$Academic_Year,1,4),"-",substr(data$Academic_Year,5,9))

  return(data)
}

get_supplemental<-function(semester, max_academic_year,department,discipline){
  max_academic_year=gsub("-", "", max_academic_year)#Academic year is in format year-year not yearyear
  
  min_academic_year<-get_min_years(max_academic_year)
 
  query<-paste0("select Academic_Year, ")
  
  if(!is.null(department) && department!="All Departments")
  query<-paste0(query," Department,")
  
  if(!is.null(discipline) && discipline!="All Disciplines")
    query<-paste0(query," Discipline,")
  
  
  query<-paste(query,
 "sum(case Residency_Status when 'AB 540' then 1 else 0 end) as AB_540,
  sum(BOGG) as BOGG, sum(Pell_Grant) as Pell_Grant
  from (
      select DISTINCT enrollments.Student_Id,Residency_Status,BOGG,Pell_Grant,Department, Discipline, sections2.Academic_Year
      from enrollments
      left join student_profiles on enrollments.Student_Id=student_profiles.Student_Id and
      enrollments.Year_Semester=student_profiles.Year_Semester aND enrollments.College=student_profiles.College
      left join sections2 on enrollments.Section_Number=sections2.Section_Number and 
      sections2.Year_Semester=enrollments.Year_Semester and sections2.College=enrollments.College
      where enrollments.College='E' and sections2.Academic_Year<=",max_academic_year," and sections2.Academic_Year>=",min_academic_year)
    if(!is.null(semester))
      query<-paste0(query," and sections2.Semester=",as.integer(sem_to_num(semester)))
  
  if(!is.null(department) && department!="All Departments")
    query<-paste0(query," and Department='",department,"'")
  
  if(!is.null(discipline) && discipline!="All Disciplines")
    query<-paste0(query," and Discipline='",discipline,"'")
  
  query<-paste0(query,
     " and Grade!='E' and sections2.Status='Open') as result
      Group By Academic_Year")
  
  if(!is.null(department) && department!="All Departments")
    query<-paste0(query,",Department")
  
  if(!is.null(discipline) && discipline!="All Disciplines")
    query<-paste0(query,",Discipline")
  
   query<-paste0(query," Order By Academic_Year")
  
  #write(query,"supplemental_query.txt")
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  data <- sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  
  #get rid of NA's
  if(!is.null(data[is.na(data)]))
    data[is.na(data)]<-0
  
  #fix academic_year
  if(!is.null(data))
    data$Academic_Year= paste0(substr(data$Academic_Year,1,4),"-",substr(data$Academic_Year,5,9))
  
  return(data)
}

#will get AA/AS awards,ADT awards, Certificates over 18 units
get_success_allocation<-function(semester,max_academic_year, program){
  max_academic_year=gsub("-", "", max_academic_year)#Academic year is in format year-year not yearyear
  min_academic_year<-get_min_years(max_academic_year)
  
  query=paste0( "Select award.Academic_Year, 
		            sum(case when award.Award_Type='AA' or award.Award_Type='AS' then 1 else 0 end) as AA_AS, 
                sum(case when award.Award_Type='AT' or award.Award_Type='ST' then 1 else 0 end) as ADT,
                sum(case when award.Award_Type='C' and Required_Units>=18 then 1 else 0 end) as Certificates_Over_18
                from (
                        select Student_Id,College,Major_Code,Award_Type,Catalog_Year,
                        (case when FORMAT(Award_Date,'MM')<=6 then cast((cast(FORMAT(Award_Date,'yyyy')-1 as varchar)+ cast(FORMAT(Award_Date,'yyyy') as varchar) ) as int)
                        when FORMAT(Award_Date,'MM')>=7 then cast((cast(FORMAT(Award_Date,'yyyy') as varchar)+ cast(FORMAT(Award_Date,'yyyy')+1 as varchar) ) as int)
                        end)as Academic_Year,
                        (case when FORMAT(Award_Date,'MM')<=2 then 0
                        when FORMAT(Award_Date,'MM')>=3 and FORMAT(Award_Date,'MM')<=6 then 1
                        when FORMAT(Award_Date,'MM')>=7 and FORMAT(Award_Date,'MM')<=8 then 2
                        when FORMAT(Award_Date,'MM')>=9  then 3 end)as Semester
                        from awards 
                )as award
                left join programs on programs.Catalog_Year=award.Catalog_Year and programs.College=award.College
                and programs.Major_Code=award.Major_Code and programs.Award_Type=award.Award_Type
                where award.College='E' and award.Academic_Year<=",max_academic_year," and award.Academic_Year>=",min_academic_year)
  if(program!="All Programs")
    query<-paste0(query," and Major='",program,"'" )
              
  if(!is.null(semester))
    query<-paste0(query," and award.Semester='",sem_to_num(semester),"' " ) 

    query<-paste(query,
                "Group by award.Academic_Year
                 order by award.Academic_Year")
  
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  result <- sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  
  #fix academic_year
  if(!is.null(result))
    result$Academic_Year= paste0(substr(result$Academic_Year,1,4),"-",substr(result$Academic_Year,5,9))
  
  return(result)
  query<-paste0(
                "select enrollments.Student_Id, enrollments.Year_Semester,enrollments.Section_Number,Units,UC_Transferable,CSU_Transferable
                from enrollments 
                left join (
                            select courses.College,Year_Semester,Section_Number, Units, UC_Transferable, CSU_Transferable
                            from courses
                            left join sections2 on sections2.College=courses.College and courses.Curriculum_Id=sections2.Curriculum_Id
                            and courses.Curriculum_Version=sections2.Curriculum_Version
                          ) as results on results.Year_Semester=enrollments.Year_Semester and results.Section_Number=enrollments.Section_Number
                          and results.College=enrollments.College
                where enrollments.College='E' and Status='Active' and (Grade='A' OR Grade='B' or Grade='C' or Grade='P') 
                and enrollments.Student_Id in (
                                                select distinct awards.Student_Id
                                                from awards 
                                                left join programs on programs.Catalog_Year=awards.Catalog_Year and programs.College=awards.College 
                                                and programs.Major_Code=awards.Major_Code and programs.Award_Type=awards.Award_Type
                                                where awards.College='E' and awards.Catalog_Year<=",max_academic_year," and awards.Catalog_Year>=",min_academic_year,"
                                                and programs.Major='",program,"'
                                                )
                order by Student_Id")
  
  #must now calculate students who completed 60 UC and 60 CSU units
  
  
  
  
              
}

getActivePrograms = function(colleges = 'E', years = MAX_YEARS_TO_SHOW)
{
  #load the underlying data set
  connection = odbcConnect("OIEA_SERVER", uid, pwd)
  
  #Go back 6 academic years - use July 1 as the cutoff
  currentYear = getAcademicYear(getCurrentTerm()) - years*10000 - years
  cutoff = paste0(substr(currentYear,1,4),"-07-01")
  
  #format multiple items as vectors
  colleges = paste0("(",paste0("'",colleges,"'",collapse = ","),")")
  
  query = paste0("select Major_Code, Major
                   from
                   (
                   select Major_Code, Major,
                   ROW_NUMBER() over(partition by Major_Code order by Catalog_Year desc) as row
                   from programs
                   where College in ",colleges," and Catalog_Year >= ",currentYear," and
                   Catalog_Year <= ",getAcademicYear(getCurrentTerm())," and 
                   (Final_Term >= ",getCurrentTerm()," or Final_Term is null)
                   ) sub1 where row = 1
                  order by Major")
  
  majors = sqlQuery(connection, query, stringsAsFactors = FALSE)
  odbcClose(connection)
  
  majors$Major_Code = factor(str_pad(majors$Major_Code, 6, "left","0"))
  majors$Major = factor(majors$Major)
  
  return(majors)
}

getAcademicYear = function(term)
{
  ##Calculate the Academic year. Ex: 20152016
  acad.yr = ifelse(sapply(term, substr,5,5) %in% c(2,3), 
                   paste0(sapply(term, substr,1,4),as.integer(sapply(term, substr,1,4))+1),
                   paste0(as.integer(sapply(term, substr,1,4))-1,sapply(term, substr,1,4)))
  
  
  return(as.integer(acad.yr))
}

getCurrentTerm = function()
{
  library(RODBC)
  library(plyr)
  library(dplyr)
  
  connection = odbcConnect("OIEA_SERVER",uid, pwd)
  query = "select max(Year_Semester) as Year_Semester from terms 
  where getdate() >= Term_Start_Date
  and College = 'E'"
  result = sqlQuery(connection, query,stringsAsFactors = FALSE)
  
  odbcClose(connection)
  
  return(result$Year_Semester)
}
getDepartments = function(academic_year, semester){
  if(!is.null(academic_year)){
    academic_year=gsub("-", "", academic_year)
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
    
    query = paste0("select distinct Department
                   from sections2 
                   where College='E' and PSA = 0 and Academic_Year=",academic_year," ")
    
    if(!is.null(semester)){
       semester=sem_to_num(semester)
       query=paste(query,"and Semester=",semester)
    }
    
    query=paste(query," order by Department")
    
    departments = sqlQuery(connection, query, stringsAsFactors = FALSE)
    odbcClose(connection)
    return(append("All Departments", departments))
  }
  return (NULL)
}

getDisciplines = function(academic_year, semester,department)
{
  if(!is.null(academic_year) && !is.null(department) && department!="All Departments"){
    academic_year=gsub("-", "", academic_year)#Academic year is in format year-year not yearyear
    
 
    
    query = paste0("select distinct Discipline
                   from sections2 
                   where College='E' and PSA = 0 and Academic_Year=",academic_year," and Department='",department,"' ")
    
    if(!is.null(semester)){
      semester=sem_to_num(semester)
      query=paste(query,"and Semester=",semester)
    }
    
    query=paste(query," order by Discipline")
    
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
    discipline = sqlQuery(connection, query, stringsAsFactors = FALSE)
    odbcClose(connection)
   

    return(append("All Disciplines", discipline$Discipline))
  }
  else if(department=="All Departments"){
    return("All Disciplines")
  }
  return (NULL)
}



adjust_year<-function(semester,year){
  
  if(!is.null(semester)){
    
    if(semester==0 || semester==1 || semester=="Spring" || semester=="Winter"){#spring or winter
      year=substr(year,6,9)
    }
    else{
      year=substr(year,1,4)
    }
  }
  return (year)
  
}

sem_to_num<-function(semester){
  if(is.null(semester))
    return("Academic Year")
  
  if(semester=="Summer")
    semester=2
  if(semester=="Fall")
    semester=3
  if(semester=="Winter")
    semester=0
  if(semester=="Spring")
    semester=1
  
  return (semester)
}

get_min_years<-function(max_year)#calculate min year to aquery which is 5 years ago
{
  return(paste0(as.integer(substr(max_year,1,4))-MAX_YEARS_TO_SHOW,as.integer(substr(max_year,5,8))-MAX_YEARS_TO_SHOW))
}

