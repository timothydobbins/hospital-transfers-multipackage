DATA temp;
   INFILE 'Y:\repos\hospital-transfers-multipackage\simple.csv' delimiter = ',' MISSOVER DSD firstobs=2;
   INPUT id adm sep transfer;
   fileseq=_n_;
RUN;

PROC SORT DATA=temp;
   BY id;
RUN;

DATA temp;
   SET temp;
   BY id;
   adm_tmp = adm;
   sep_tmp = sep;

   IF first.id THEN episode=0;
   episode + 1;
RUN;

PROC TRANSPOSE DATA=temp OUT=temp_melt (RENAME = (_name_=type col1 = time));
   BY fileseq id episode transfer adm sep;
RUN;

DATA temp_melt;
   SET temp_melt;
   time_adj = time + (type="sep_tmp" & transfer=1) * 0.1;
   IF type="adm_tmp" THEN inout = 1;
   ELSE IF type="sep_tmp" THEN inout=-1;
RUN;

PROC SORT DATA=temp_melt;
   BY id time_adj episode DESCENDING inout;
RUN;

DATA t2;
   SET temp_melt;
   BY id;

   IF first.id THEN cumsum=0;
   cumsum+inout;

   newstay = (first.id=1 & cumsum=1) | lag(cumsum=0);

   IF first.id THEN stayseq=0;
   stayseq + newstay;
RUN;

* Turn data from long to wide;
PROC SORT DATA=t2;
   BY id stayseq episode transfer;
RUN;
PROC TRANSPOSE DATA=t2 OUT=t3 (DROP=_name_ RENAME=(adm_tmp=adm sep_tmp=sep));
   BY id stayseq episode transfer;
   VAR time;
   ID type;
RUN;

* Calculate transseq;
DATA t3;
   SET t3;
   BY id stayseq;

   IF first.stayseq THEN transseq=0;
   ELSE transseq+1;
RUN;

* Obtain final date of separation within a stay and populate to all records;
PROC SUMMARY DATA=t3;
   CLASS id stayseq;
   VAR sep;
   WAYS 2;
   OUTPUT OUT=maxsep (DROP = _type_ _freq_) MAX=sepdate_fin;
RUN;

DATA t4;
   MERGE t3 maxsep;
   BY id stayseq;

   totlos = max((sepdate_fin - adm), 1);
RUN;

PROC PRINT DATA=t4;
RUN;
