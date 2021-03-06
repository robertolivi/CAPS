;*************************************************
;CAPS_BINARY Binary Initialize
;*************************************************
function caps_binary::init

  ;-- allocate memory to pointer when initializing object
  self.const=ptr_new(/allocate)   
;  *(self.const)=temp
  return,1
   
end



;*************************************************
;Load Constants
;*************************************************
function caps_binary::load_constants

  constants=*(self.const)
  return, constants

end





;*************************************************
;Check if table exists in MySQL 
;*************************************************
function caps_binary::check_level1_size,type,yd
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  tb_check=type+'_'+yd+'_LEVEL1'
  dbloc="jdbc:mysql://localhost:3306/CAPS_LEVEL1"
  oMySql = obj_new('MySql', dbloc, "root", "")
  tb=oMySql->QUERY('SHOW TABLES')
  obj_destroy, oMySql
  if size(tb, /type) eq 8 then tb=tb.tables_in_caps_level1 $
  else tb='NONE'
  pp=where(tb EQ tb_check,result)
  if result eq 0 then return, 10. $
  else begin
     oMySql = obj_new('MySql', dbloc, "root", "")
     temp=oMySql->QUERY("SELECT id from "+tb_check+";")
     obj_destroy, oMySql
     if size(temp,/type) ne 8 then return, 0. $
     else return, n_elements(temp)
  endelse
end



;*************************************************
;Check if table exists in MySQL 
;*************************************************
function caps_binary::check_level,type,yd,level
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)
  tb_check=type+'_'+yd+'_'+level
  print, 'Check if '+tb_check+' exists.'
  dbuser="root"
  dbpass=""
  dbloc="jdbc:mysql://localhost:3306/CAPS_"+level
  oMySql = obj_new('MySql', dbloc, dbuser, dbpass)
  tb=oMySql->QUERY('SHOW TABLES')
  obj_destroy, oMySql
  if size(tb, /type) eq 8 then cmd="tb=tb.tables_in_caps_"+level $
  else cmd="tb='NONE'"
  temp=execute(cmd)
  pp=where(tb EQ tb_check,result)
  IF result EQ 1 THEN print, tb_check+' exists' $
  else print, tb_check+' does not exist.' 
  return, result
end








;*************************************************
;Check if binaries exist 
;*************************************************
function caps_binary::check_files,type,yd
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
function caps_binary::load_binary,type,yd
  ;type  -> string (SNG, ELS, etc.)
  ;yd    -> string (2004181,...)
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
        if size(data_level1,/type) eq 0 then data_level1=temp ELSE $
           data_level1=[data_level1,temp]
     endif
  endfor
  close, /all
  return, data_level1

end



;*************************************************
;Test CAPS Binary
;*************************************************
function caps_binary::test_caps_binary
  ;type  -> string (SNG, ELS)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)
  type='SNG'
  yd='2004182'
  level='LEVEL1'

  ;--------------------------------
  temp=self->check_files(type,yd)

  ;--------------------------------
  temp=self->check_level(type,yd,level)

  ;--------------------------------
  print, 'Load binary'
  data_level0=self->load_binary(type,yd)

  ;-------------------------------
  print, 'Drop table level1'
  drop_table,"CAPS_LEVEL1",type+"_"+yd+"_"+level

  ;-------------------------------
  print, 'Create table level1'
  create_table_level1,type,yd

  ;-------------------------------
  print, 'Insert into level1'
  insert_caps_data_level1,type,yd,data_level0

end




;*************************************************
;Test CAPS Binary
;*************************************************
function caps_binary::level1, type,yd
  ;type  -> string (SNG, ELS)
  ;yd    -> string (2004181,...)
  ;level -> string (LEVEL0, LEVEL1)

  ;-------------------------------
  print, 'Drop table level1'
  drop_table,"CAPS_LEVEL1",type+"_"+yd+"_LEVEL1"


  ;--------------------------------
  ;Check elements in level1
  temp=self->check_level1_size(type,yd)
  if temp eq 0 then goto, doit
  print,temp
  
  ;--------------------------------
  ;Check if level1 mysql table exists
  temp=self->check_level(type,yd,'LEVEL1')
  if temp eq 1 then goto, skip

  ;--------------------------------
  ;Check if level0 binary files exists
  temp=self->check_files(type,yd)
  if total(temp) eq 0 then $
     ims_download_phoebe,strmid(yd,0,4),strmid(yd,4,3)

  ;-------------------------------
  print, 'Create table level1'
  create_table_level1,type,yd

  doit: ;In case the table exists but is empty
  ;--------------------------------
  ;Load Binaries
  print, 'Load binary'
  data_level0=self->load_binary(type,yd)     

  if size(data_level0,/type) ne 8 then goto, skip

  ;-------------------------------
  print, 'Insert into level1'
  insert_caps_data_level1,type,yd,data_level0
  skip:
end





;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------


pro caps_binary__define

  void={caps_binary,const:ptr_new()}
  return

end
