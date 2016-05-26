;*************************************************
;CAPS_BINARY_SQLITE Binary Initialize
;*************************************************
function caps_binary_sqlite::init

  ;-- allocate memory to pointer when initializing object
  self.const=ptr_new(/allocate)   
  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/sng/caps_sng_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
;  *(self.const)=temp
  return,1
   
end



;*************************************************
;Load Constants
;*************************************************
function caps_binary_sqlite::load_constants

  constants=*(self.const)
  return, constants

end





;*************************************************
;Check if table exists in MySQL 
;*************************************************
function caps_binary_sqlite::check_table,type,yd,level
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)
  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/'+type+'/caps_'+type+'_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
  print,'------->Checking if table exists'
  tb_check=type+'_'+yd+'_LEVEL1'
  cmd="SELECT name FROM sqlite_master "+$
      "WHERE type='table' AND name='"+tb_check+"' ;"
  if size(get_sql(cmd,/string),/type) eq 7 then begin
     print, 'Table '+tb_check+' exists.'
     return,1
  endif else begin
     print, 'Table '+tb_check+' does not exist.'
     return,0
  endelse
end





;*************************************************
;Create table in MySQL 
;*************************************************
function caps_binary_sqlite::create_table_level1,type,yd
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/'+type+'/caps_'+type+'_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
  print, '------> Create table '+type+"_"+yd+"_LEVEL1
  return,"CREATE TABLE `"+type+"_"+yd+"_LEVEL1` ("+$
;         "id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,"+$
         "UTC VARCHAR(26), "+$
         "ET double, "+$
         "BCYC INT, "+$
         "ACYC INT, "+$
         "TLM INT, "+$
         "OFFSET INT,"+$
         "A1 VARCHAR(1000), "+$
         "A2 VARCHAR(1000), "+$
         "A3 VARCHAR(1000), "+$
         "A4 VARCHAR(1000), "+$
         "A5 VARCHAR(1000), "+$
         "A6 VARCHAR(1000), "+$
         "A7 VARCHAR(1000), "+$
         "A8 VARCHAR(1000)"+$
         ");"
end


;*************************************************
;Drop Sqlite table
;*************************************************
function caps_binary_sqlite::drop_table,type,yd,level
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/'+type+'/caps_'+type+'_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
  print, '-------> Drop table '+type+'_'+yd+'_'+level
  return,"DROP TABLE "+type+'_'+yd+'_'+level+";"
end









;*************************************************
;Check if binaries exist 
;*************************************************
function caps_binary_sqlite::check_files,type,yd
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  dir='/Volumes/rob/data/cassini/caps/binary/'
  file_location = $                  
     [dir+type+'/'+type+'_'+yd+'00_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'06_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'12_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'18_U3.DAT']  
  ;/* ------- Check if files exist -------- */
  result=[file_test(file_location[0]),$
          file_test(file_location[1]),$
          file_test(file_location[2]),$
          file_test(file_location[3])] 
  return, result
end










;*************************************************
;Load Binary
;*************************************************
function caps_binary_sqlite::load_binary,type,yd
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  print, '------> Load binaries'
  dir='/Volumes/rob/data/cassini/caps/binary/'
  file_name=$
     [type+'_'+yd+'00_U3.DAT',$
      type+'_'+yd+'06_U3.DAT',$
      type+'_'+yd+'12_U3.DAT',$
      type+'_'+yd+'18_U3.DAT']
  file_location = $                  
     [dir+type+'/'+type+'_'+yd+'00_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'06_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'12_U3.DAT',$
      dir+type+'/'+type+'_'+yd+'18_U3.DAT']  
  loc=[file_test(file_location[0]),$
       file_test(file_location[1]),$
       file_test(file_location[2]),$
       file_test(file_location[3])] 
  summ=total(loc)

  ;/* ------- Check file lengths -------- */
  file_info1=file_info(file_location[0])
  file_info2=file_info(file_location[1])
  file_info3=file_info(file_location[2])
  file_info4=file_info(file_location[3])
  
  file_length=[file_info1.size,$
               file_info2.size,$
               file_info3.size,$
               file_info4.size]/40L
  
  bcyc=uint(0)                  ;IDL 16 bit unsigned int
  acyc=uint(0)                  ;IDL 16 bit unsigned int
  time=double(-1e20)            ;IDL 64 bit float
  tlm=byte(0)                   ;IDL 8 bit unsigned int
  spare=byte(0)                 ;IDL 8 bit unsigned int
  off=uint(0)                   ;IDL 16 bit unsigned int
  en=uintarr(2)                 ;2 IDL 16 bit unsigned ints
  az=uintarr(2)                 ;2 IDL 16 bit unsigned ints
  dat=uintarr(8)                ;8 IDL 16 bit unsigned ints

  if total(loc) eq 0 then return, 0
  for i_file=0, 3 do begin

     ;if file is empty or does not exist
     ;then skip file
     if loc[i_file] ne 0 then begin
        openr,lun,file_location[i_file], $
              /SWAP_IF_LITTLE_ENDIAN,$
              /get_lun
        temp=replicate({bcyc:uint(0),$
                        acyc:uint(0),$
                        time:double(-1e20),$
                        tlm:byte(0),$
                        spare:byte(0),$
                        off:uint(0),$
                        en:uintarr(2),$
                        az:uintarr(2),$
                        dat:uintarr(8),$
                        dat_s:string(0)},$
                       file_length[i_file])
        
        print, 'Load Binary from ',+file_name[i_file]
        jj=0.
        while not(EOF(lun)) do begin
           readu,lun,bcyc
           readu,lun,acyc
           readu,lun,time
           readu,lun,tlm
           readu,lun,spare
           readu,lun,off
           readu,lun,en
           readu,lun,az
           readu,lun,dat
           _dat1=string(dat,format='(F9.4)')
           _dat2=strjoin(_dat1,',')
           temp[jj]={bcyc:bcyc,$
                     acyc:acyc,$
                     time:time,$
                     tlm:tlm,$
                     spare:spare,$
                     off:off,$
                     en:en,$
                     az:az,$
                     dat:dat,$
                     dat_s:_dat2}
           jj++
        endwhile
        if size(data_level0,/type) eq 0 then data_level0=temp ELSE $
           data_level0=[data_level0,temp]
     endif
  endfor
  close, /all
  return, data_level0

end


;*************************************************
;Insert CAPS binary data into sqlite3
;*************************************************
function caps_binary_sqlite::insert_caps_data_level1,type,yd,data_level0

  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/'+type+'/caps_'+type+'_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
  print, '------>Convert Level0 to Level1.'
  tb1=type+'_'+yd+'_LEVEL1'
  data_level0.time=data_level0.time+data_level0.off/1000.D
  nn=n_elements(data_level0)
  cts=rebin(replicate(65535.D,63),63,8,round(nn/30.))
  index=replicate(-1.,round(nn/30.))
  k=0.
  for i=0., n_elements(data_level0)-2L do begin
     ;counts=fltarr(63,8)
     j=0.
     ttemp=0
     while data_level0[i].en[0] ne 64 do begin
        if data_level0[i].en[0] gt ttemp then $
           ttemp=data_level0[i].en[0] $
        else begin
           i--
           goto, escape
        endelse        
        cts[j,*,k]=$
           [data_level0[i].dat[0],$
            data_level0[i].dat[1],$
            data_level0[i].dat[2],$
            data_level0[i].dat[3],$
            data_level0[i].dat[4],$
            data_level0[i].dat[5],$
            data_level0[i].dat[6],$
            data_level0[i].dat[7]]/$
           (data_level0[i].en[1]-data_level0[i].en[0]+1.)/$
           (data_level0[i].az[1]-data_level0[i].az[0]+1.)
        if (data_level0[i].en[1] eq 63) or $
           (i eq n_elements(data_level0)-1) then goto, escape
        i++
        j++
     endwhile
     escape:
     index[k]=i-j
     k=k+1.
  endfor
  index=index[0:k-1]
  cts=cts[*,*,0:k-1]  
  orig_index=index
  nn=n_elements(index)
  print, '------>Create insert string.'
  ;-----------------------------------------------------
  ;Insert Data
  lim=500;Limit imposed by SQLITE3
  print, '------>Insert data into '+type+'_'+yd+'_Level1.'
  for ij=0, nn-1,lim do begin
     if ij+lim ge nn then pp=nn-1 else pp=ij+lim-1
     index=orig_index[ij:pp]
     et=strtrim(string(data_level0[index].time),2)
     cspice_et2utc,data_level0[index].time,'isod',6,utc
     bcyc=strtrim(string(data_level0[index].bcyc),2)
     acyc=strtrim(string(data_level0[index].acyc),2)
     tlm=strtrim(string(fix(data_level0[index].tlm)),2)
     off=strtrim(string(fix(data_level0[index].off)),2)
     cts2=strjoin($
          transpose($
          reform($
          strtrim($
          string(cts[*,*,ij:pp],format='(F12.2)'),2),63,8,pp-ij+1),[0,2,1]),',')
     values=[$
            ["('"+utc+"'"],[et],[bcyc],[acyc],[tlm],[off],$
            ["'"+reform(cts2[*,0])+"'"],["'"+reform(cts2[*,1])+"'"],$
            ["'"+reform(cts2[*,2])+"'"],["'"+reform(cts2[*,3])+"'"],$
            ["'"+reform(cts2[*,4])+"'"],["'"+reform(cts2[*,5])+"'"],$
            ["'"+reform(cts2[*,6])+"'"],["'"+reform(cts2[*,7])+"')"]]
     values=strjoin(reform(transpose(values),n_elements(values)),",")  
     temp="INSERT INTO `"+tb1+"` "+$
          "(utc,et,bcyc,acyc,tlm,offset,a1,a2,a3,a4,a5,a6,a7,a8)"+$
          " VALUES "+values+";"
     exec_sql,temp
  endfor
  data_level0=0
  return,temp
  
end









;*************************************************
;Test CAPS Binary
;*************************************************
function caps_binary_sqlite::test_caps_binary_sqlite
  ;type  -> string (SNG, ELS)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)
  load_kernels
  t1=systime(1)
  type='SNG'
  yd='2004182'
  level='LEVEL1'
  tb=type+'_'+yd+'_'+level
  temp=self->check_files(type,yd);--------------------------------Check binary file
  temp=self->check_table(type,yd,level);--------------------------Check level1 table
  exec_sql,self->drop_table(type,yd,level);-----------------------Drop table
  exec_sql,self->create_table_level1(type,yd);--------------------Create table
  data_level0=self->load_binary(type,yd);-------------------------Load Binary
  string=self->insert_caps_data_level1(type,yd,data_level0);----Insert data
  exec_sql,string
  t1=systime(2)
  print, 'Time: ',t1-t1,' seconds'
  cspice_kclear

end










;*************************************************
;CAPS Binary
;*************************************************
function caps_binary_sqlite::level1, type,yd
  ;type  -> string (SNG, ELS)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)
  ;-------------------------------
  load_kernels
  prot='jdbc:sqlite:'
  dirr='/Volumes/rob/data/cassini/caps/sqlite/'+type+$
       '/caps_'+type+'_level1.db'
  defsysv, '!_IDL_SQL_URL',prot+dirr
  temp=self->check_files(type,yd);--------------------------------Check Binary File
  if total(temp) eq 0 then goto, skip;---------------------------------------------
  temp=self->check_table(type,yd,level);--------------------------Check Level1 Table
  if total(temp) ne 0 then exec_sql,self->drop_table(type,yd,'LEVEL1');------Drop table
  exec_sql,self->create_table_level1(type,yd);--------------------Create table
  data_level0=self->load_binary(type,yd);-------------------------Load Binary
  if size(data_level0,/type) ne 8 then goto, skip;----------------Check Binary Load
  string=self->insert_caps_data_level1(type,yd,data_level0);----Insert data
  exec_sql,string
  ;exec_sql,self->insert_caps_data_level1(type,yd,data_level0);----Insert data
  cspice_kclear
  skip:

end





;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------


pro caps_binary_sqlite__define

  void={caps_binary_sqlite,const:ptr_new()}
  return

end
