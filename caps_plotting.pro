;INPUT: type='SNG', et=double (ex:[seconds])

pro caps_plotting;, type, et


  ;/*****************************
  cspice_utc2et,'2005-047T22:00:00',minn
  cspice_utc2et,'2005-047T23:00:00',maxx
;  cspice_utc2et,'2005-047T09:00:00',minn
;  cspice_utc2et,'2005-047T23:51:00',maxx
  type='SNG'
  utc='2005-047T00:00:00.000'
  cspice_utc2et, utc, et
  et=et+findgen(60.*60.*24.)
  et_tot=et
  cspice_et2utc,et,'isod',6,utc
  year=strmid(utc,0,4)
  day=strmid(utc,5,3)
  yd=float(year+day)
  h=histogram(yd,reverse_indices=ri)
  nn=where(h ne 0,cc)
  ;/*****************************

  ;Load ANC Binary
  binary=ims_binary_anc(year[0],day[0])
  if size(binary,/type) ne 8 then stop
  pp=where(((binary.hvu2_st_dac GE -2500.) OR $
            (binary.hvu2_st_dac LT -2630.)) AND $
           ((binary.ims_sweep_table_number EQ 1) OR $
            (binary.ims_sweep_table_number EQ 2)),ccc)  



  ;---------------------------------------------------
  ;Check if table exists
  print,'------->Checking if table exists'  
  tb_check=type+'_'+year[0]+day[0]+'_LEVEL1'
  cmd="SELECT name FROM sqlite_master "+$
      "WHERE type='table' AND name='"+tb_check+"' ;"
  if size(get_sql(cmd),/type) ne 11 then print, 'Table '+tb_check+' exists.' $
  else print, 'Table '+tb_check+' does not exist.'

  ;---------------------------------------------------
  ;Load data from table
  print, '-------->Load data from '+tb_check
  cmd="SELECT et,en_start,en_end,az_start,az_end,tlm FROM "+tb_check+";"
  var=get_sql(cmd)
  cmd="SELECT a1,a2,a3,a4,a5,a6,a7,a8 FROM "+tb_check+";"
  sng=get_sql(cmd,/string)

  ;--------------------------------------------------
  ;Create time intervals
  caps=obj_new('caps')
  num=caps->cts_arr(n_elements(var[0,*]))
  num.a1=reform(sng[0,*])
  num.a2=reform(sng[1,*])
  num.a3=reform(sng[2,*])
  num.a4=reform(sng[3,*])
  num.a5=reform(sng[4,*])
  num.a6=reform(sng[5,*])
  num.a7=reform(sng[6,*])
  num.a8=reform(sng[7,*])
  counts=mysql_to_counts(num)
  obj_destroy, caps


  ;------------------------------------------------
  ;Restore Plasma Parameters
  bck_load_ims_single_sav,$
     '~/idl/projects/fitting/data/ims/ims_2005047_2m_1d.sav',sng
  IF mean(temp.r) lt 1 then temp.r=temp.r*60268.
  IF mean(temp.r) gt 60268. then temp.r=temp.r/60268.
  ppp=where(temp.r EQ min(temp.r))
  temp.r[0:ppp[0]]=(-1.)*temp.r[0:ppp[0]]
  stop
  
  
  et=dblarr(2,n_elements(var[0,*]))
  enrg=dblarr(2,63)
  int1=( var[3,*]-1. + (var[1,*]-1.)/64.)*4.
  int2=( var[4,*]-1. + (var[2,*])/64.)*4.
  et[0,*] = var[0,*] + ( var[3,*]-1 + (var[1,*]-1.)/64.)*4.
  et[1,*] = var[0,*] + ( var[4,*] + (var[2,*])/64.)*4.





  ;****************************************************************************
  ;Setup Plot
  pos1=[0.1,0.3,0.9,1.0]
  pos2=[0.1,0.0,0.9,0.3]

  enrg[0,*]=findgen(63)
  enrg[1,*]=findgen(63)+1.
  device, decomposed=0
  cgloadct,39
  ppp=where(et[0,*] ge minn and et[0,*] lt maxx)
  et=et[*,ppp]
  clr=bytscl($
      reform($
      alog10($
      mean(counts[*,*,ppp],dimension=1))),min=alog10(300.), max=alog10(600.))
  cspice_et2utc,reform(var[0,*]),'isod',6,utccc
  tbl=interpol(binary.ims_sweep_table_number,binary.time,reform(var[0,*]))
  dac=interpol(binary.hvu2_st_dac,binary.time,reform(var[0,*]))
  cgLOADCT,39 ,clip=[0,254]
  window,/free, xsize=660, ysize=200
  device, decomposed=0
  plot, [0,1],[0,1], $
        xrange=[minn,maxx], $
        yrange=[2,63],/nodata,xstyle=5,ystyle=5,$
        position=[0.0,0.0,1.0,1.0],xtickname=replicate(' ',10),$
        xticklen=0.
  cgloadct,0
  polyfill, $
     [minn,maxx,maxx,minn,minn],$
     [2,2,63,63,2],color=230
  cgloadct,39
  for i=0, n_elements(et[0,*])-1 do $
  for j=2, 62 do $
     polyfill, $
     [et[0,i],et[0,i],et[1,i],et[1,i],et[0,i]],$
     [enrg[0,j],enrg[1,j],enrg[1,j],enrg[0,j],enrg[0,j]],$
     color=clr[j,i]
  image=cgSnapshot(FILENAME='test.jpg',/true)
  cgLOADCT, 39
  SET_PLOT, 'PS'
  DEVICE,/color,$
         /inches, $
         XSIZE=6.6, $
         YSIZE=4.,$
         YOFFSET=7, $
         XOFFSET=1,$
         /HELVETICA,$
         filename='~/idl/projects/background/plots/spec_test.eps'
  !P.FONT=0.
  !X.THICK=4.
  !Y.THICK=4.
  !P.CHARSIZE=0.8
  !P.THICK=1.5
  TVIMAGE, image, position=pos1
  year=strmid(utc,0,4)
  day=strmid(utc,5,3)
  hh=strmid(utc,9,2)
  mm=strmid(utc,12,2)
  ss=strmid(utc,15,2)
  time=hh+':'+mm
  pp=findgen(24.*4.)*15.*60.+1.+60.*10.
  xx=et_tot
  nn=n_elements(xx)
  plot, [0,1],[0,1], $
        xrange=[minn,maxx], $
        yrange=[0,1],/nodata,xstyle=5,ystyle=5,$
        position=pos2,/noerase
  yy1=replicate(0.75,nn)
  yy2=replicate(0.50,nn)
  yy3=replicate(0.25,nn)
  nn=n_elements(pp)
  eph=obj_new('caps_ephemeris')
  lshell=eph->lshell(et_tot[pp])
  rs=eph->radial(et_tot[pp])
  obj_destroy,eph
  rs=strtrim(string(rs/60268.,format='(F6.3)'),2)
  lshell=strtrim(string(lshell,format='(F5.2)'),2)
  for i=0, nn-1 do begin
     if xx[pp[i]] ge minn AND xx[pp[i]] le maxx then begin
        oplot, [xx[pp[i]],xx[pp[i]]], [0.9,1.0]
        XYOUTS, xx[pp[i]], yy1[pp[i]], time[pp[i]],alignment=0.5
        XYOUTS, xx[pp[i]], yy2[pp[i]], rs[i],alignment=0.5
        XYOUTS, xx[pp[i]], yy3[pp[i]], lshell[i],alignment=0.5
        
     endif  
  endfor
  device, /close
  set_plot, 'X'

  stop

end
