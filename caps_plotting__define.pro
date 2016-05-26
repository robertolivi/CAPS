;INPUT: type='SNG', et=double (ex:[seconds])

pro caps_plotting;, type, et

  utc=[$
      '2004-156T00:00:00.000',$
      '2004-156T01:00:00.000',$
      '2004-156T02:00:00.000',$
      '2005-156T01:00:00.000',$
      '2005-156T02:00:00.000',$
      '2005-156T03:00:00.000',$
      '2005-156T04:00:00.000',$
      '2005-156T05:00:00.000',$
      '2005-156T06:00:00.000',$
      '2005-156T00:00:00.000',$
      '2006-156T00:00:00.000',$
      '2007-156T00:00:00.000',$
      '2008-156T00:00:00.000',$
      '2009-156T00:00:00.000']
  

  cspice_et2utc,et,'isod',6,utc
  year=strmid(utc,0,4)
  day=strmid(utc,5,3)
  h=histogram(day,reverse_index=ri)
  stop
;  for i=0., n_elements(h)-1 do if ri[i+1] gt ri[i] then $
;     tt[i+om=[ri[ri[i]:ri[i+1]-1]]


  ;---------------------------------------------------
  ;Check if table exists
  print,'------->Checking if table exists'
  tb_check=type+'_'+year+day+'_LEVEL1'
  cmd="SELECT name FROM sqlite_master "+$
      "WHERE type='table' AND name='"+tb_check+"' ;"
  if size(get_sql(cmd,/string),/type) ne 3 then $
     print, 'Table '+tb_check+' exists.' $
  else $
   print, 'Table '+tb_check+' does not exist.'
  stop




end
