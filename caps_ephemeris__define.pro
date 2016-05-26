;*************************************************
;CAPS_EPHEMERIS Initialize
;*************************************************

function caps_ephemeris::init

;-- allocate memory to pointer when initializing object

  self.TARGET_NAME = ptr_new(/allocate)                                 
  self.SC_NAME = ptr_new(/allocate)
  self.SC_ID = ptr_new(/allocate)
  self.SC_BUS = ptr_new(/allocate)
  self.tol_sec = ptr_new(/allocate)
  *(self.target_name) = 'Saturn Barycenter'
  *(self.SC_name) = 'Cassini'
  *(self.SC_ID)   = -82
  *(self.SC_Bus)  = -82000
  *(self.tol_sec) =5.

  return,1

end



;*************************************************
;Select Target
;*************************************************
function caps_ephemeris::target,target

  *(self.target_name)=target
  return,1

end


;*************************************************
;Select Observer
;*************************************************
function caps_ephemeris::observer,observer

  *(self.sc_name)=observer
  return,1

end





;*************************************************
;Time Evaluate
;*************************************************
function caps_ephemeris::time_evaluate,time

  ;-----------------------------------------------
  ;Check ET
;  print, 'Check ET'
  if size(time,/type) eq 7 then $
     cspice_str2et, time, et $
  else et=time

  self.v_et=ptr_new(/allocate)
  *(self.v_et)=et

  return, et

end







;*************************************************
;J2000
;*************************************************
function caps_ephemeris::j2000,time
  
  if not self.v_et then temp=self.time_evaluate(time)  

  ;---Coordinates J2000 between Cassini-Saturn
  cspice_spkezr, *(self.sc_name), *(self.v_et), 'J2000', 'NONE', $
                 *(self.target_name), $
                 state1, ltime1_2
  ;---Coordinates J2000 between Cassini-Sun
  cspice_spkezr, *(self.sc_name), *(self.v_et), 'J2000', 'NONE', $
                 'SUN', $
                 state2, ltime2_2
  j2000={state1:state1,state2:state2}
  self.v_j2000=ptr_new(/allocate)
  *(self.v_j2000)=j2000
  return, j2000

end






;*************************************************
;Radial Distance
;*************************************************
function caps_ephemeris::radial,time

;  print, 'Calculate radial distance (km)'

  if not self.v_j2000 then temp=self.j2000(time)
  j2000=*(self.v_j2000)
  radial=sqrt(TOTAL(j2000.state1[0:2,*]^2,1))
  self.v_r=ptr_new(/allocate)
  *(self.v_r)=radial
  return, radial

end







;*************************************************
;Co-Latitude Distance
;*************************************************
function caps_ephemeris::colatitude,time

  ;Check time
  if not self.v_j2000 then temp=self.j2000(time)
  if not self.v_r then temp=self.radial(time)
  j2000=*(self.v_j2000)
  r=*(self.v_r)
  ;Check radial distance
  print, 'Calculate latitude [degrees]'
  sp=[0.0854813583, 0.0732355383, 0.9936445508]
  lat=reform(90. - !radeg*ACOS( $
      (sp[0]*j2000.state1[0,*] +$
       sp[1]*j2000.state1[1,*]+$
       sp[2]*j2000.state1[2,*]) / r ))
  self.v_lat=ptr_new(/allocate)
  *(self.v_lat)=lat
  return, lat

end






;*************************************************
;Local Time
;*************************************************
function caps_ephemeris::localtime,time

  if not self.v_j2000 then temp=self.j2000(time)
  j2000=*(self.v_j2000)
  print, 'Calculate Local Time'
  sp=[0.0854813583, 0.0732355383, 0.9936445508]
  SunPOS=[ j2000.state1[0,*]-j2000.state2[0,*], $
           j2000.state1[1,*]-j2000.state2[1,*], $
           j2000.state1[2,*]-j2000.state2[2,*]]
  RhoSP=[ SP[1]*SunPOS[2,*]-SP[2]*SunPOS[1,*],$
          SP[2]*SunPOS[0,*]-SP[0]*SunPOS[2,*],$
          SP[0]*SunPOS[1,*]-SP[1]*SunPOS[0,*]]
  phiSP= atan(RhoSP[1,*], RhoSP[0,*])  
  RhoSC=[ SP[1]*j2000.state1[2,*]-SP[2]*j2000.state1[1,*],$
          SP[2]*j2000.state1[0,*]-SP[0]*j2000.state1[2,*]]
  phiSC= atan(RhoSC[1,*], RhoSC[0,*])
  ltime=(phiSC-phiSP+!PI)*12./!PI
  nn=where(ltime LT 0.,cc)
  if cc ne 0. then ltime[nn]=24+ltime[nn] $
  else ltime= ltime mod 24
  ltime=reform(ltime)
  self.v_ltime=ptr_new(/allocate)
  *(self.v_ltime)=ltime
  return, ltime

end







;*************************************************
;EQU
;*************************************************
function caps_ephemeris::equ,time

  if not self.v_ltime then temp=self.localtime(time)
  if not self.v_lat then temp=self.colatitude(time)
  if not self.v_r then temp=self.radial(time)
  ltime=*(self.v_ltime)
  lat=*(self.v_lat)
  r=*(self.v_r)
  print, 'Calculate Local Time'
  phi=(360.*ltime/24.-180.)*!dtor
  EQU_X=r*cos(LAT*!dtor)*cos(phi)/60268.
  EQU_Y=r*cos(LAT*!dtor)*sin(phi)/60268.
  EQU_Z=r*sin(LAT*!dtor)/60268.
  EQU=[[EQU_X],[EQU_Y], [EQU_Z]]

  self.v_equ=ptr_new(/allocate)
  *(self.v_equ)=equ
  return, equ

end





;*************************************************
;KSM
;*************************************************
function caps_ephemeris::ksm,time


  if not self.v_j2000 then temp=self.j2000(time)
  if not self.v_equ then temp=self.equ(time)
  j2000=*(self.v_j2000)
  equ=*(self.v_equ)
  equ_y=reform(equ[*,1])

  print, 'Calculate Local Time'
  sp=[0.0854813583, 0.0732355383, 0.9936445508]
  SunPOS=[ j2000.state1[0,*]-j2000.state2[0,*], $
           j2000.state1[1,*]-j2000.state2[1,*], $
           j2000.state1[2,*]-j2000.state2[2,*]]
  RhoSP=[ SP[1]*SunPOS[2,*]-SP[2]*SunPOS[1,*],$
          SP[2]*SunPOS[0,*]-SP[0]*SunPOS[2,*],$
          SP[0]*SunPOS[1,*]-SP[1]*SunPOS[0,*]]
  phiSP= atan(RhoSP[1,*], RhoSP[0,*])  
  RhoSC=[ SP[1]*j2000.state1[2,*]-SP[2]*j2000.state1[1,*],$
          SP[2]*j2000.state1[0,*]-SP[0]*j2000.state1[2,*]]
  phiSC= atan(RhoSC[1,*], RhoSC[0,*])
  angle=acos((SunPOS[0,*]*j2000.state1[0,*]+$
              SunPOS[1,*]*j2000.state1[1,*]+$
              SunPOS[2,*]*j2000.state1[2,*]) / $
             (sqrt(SunPOS[0,*]^2+$
                   SunPOS[1,*]^2+$
                   SunPOS[2,*]^2)*$
              sqrt(j2000.state1[0,*]^2+$
                   j2000.state1[1,*]^2+$
                   j2000.state1[2,*]^2)))
  KSM_X=sqrt(j2000.state1[0,*]^2+$
             j2000.state1[1,*]^2+$
             j2000.state1[2,*]^2)*$
        cos(angle)/60286.
  KSM_Y=EQU_Y
  VECTOR=[RhoSP[1,*]*SunPOS[2,*]-RhoSP[2,*]*SunPOS[1,*],$
          RhoSP[2,*]*SunPOS[0,*]-RhoSP[0,*]*SunPOS[2,*],$
          RhoSP[0,*]*SunPOS[1,*]-RhoSP[1,*]*SunPOS[0,*]]  
  angle=acos((vector[0,*]*j2000.state1[0,*]+$
              vector[1,*]*j2000.state1[1,*]+$
              vector[2,*]*j2000.state1[2,*])/$
             (sqrt(vector[0,*]^2+$
                   vector[1,*]^2+$
                   vector[2,*]^2)*$
              sqrt(j2000.state1[0,*]^2+$
                   j2000.state1[1,*]^2+$
                   j2000.state1[2,*]^2)))
  KSM_Z=sqrt(j2000.state1[0,*]^2+$
             j2000.state1[1,*]^2+$
             j2000.state1[2,*]^2)*$
        cos(angle)/60286.*(-1.) ;(-1) due to flipping of cross     
  ksm_x=reform(ksm_x)
  ksm_y=reform(ksm_y)
  ksm_z=reform(ksm_z)
  ksm=[[ksm_x],[ksm_y],[ksm_z]]
  return, ksm

end







;*************************************************
;LSHELL
;*************************************************
function caps_ephemeris::lshell,time

  if not self.v_et then temp=self.time_evaluate(time)  

  print, 'Calculate LSHELL'
  CSPICE_SPKEZR,'Cassini',*(self.v_et),'IAU_SATURN','NONE','Saturn',cas,owlt
  CSPICE_SPKEZR,'Sun',*(self.v_et),'IAU_SATURN','NONE','Saturn',sun,ltime2
  SM=fltarr(3,n_elements(time))
  SM_V=fltarr(3,n_elements(time))
  FOR i=0,n_elements(time)-1 DO BEGIN
     CSPICE_TWOVEC,[0,0,1],3,sun[0:2,i],1,rot
     SM[*,i]=rot##cas[0:2,i]
     SM_V[*,i]=rot##cas[3:5,i]
  ENDFOR  
  rs=60268.0D
  SM=SM/rs
  rr=sqrt(total(SM[0:2,*]^2,1))
  lat=atan(SM[2,*]/sqrt(SM[0,*]*SM[0,*]+SM[1,*]*SM[1,*]))
  LSHELL=rr/(cos(lat)*cos(lat))
  
  self.v_lshell = ptr_new(/allocate)
  *(self.v_lshell)=lshell
  return, lshell
  
end









;*************************************************
;SLS3
;*************************************************
function caps_ephemeris::sls3,time

  ;The SLS3 longitude system is only valid for this time period:
  ;1 January 2004 through 10 August 2007.
  cspice_utc2et, '2004-001T00:00:00.000',et1
  cspice_utc2et, '2007-222T00:00:00.000',et2

  if not self.v_et then temp=self.time_evaluate(time)  
  if not self.v_ltime then temp=self.localtime(time)  
  et=*(self.v_et)
  ltime=*(self.v_ltime)

  print, 'Calculate SLS3'
  coefs=[86.6681D0, -2.7537D0, 0.0047730D, $
         -4.8755D-6, 3.5653D-9, -9.1485D-13]
  owlt=8.69
  cspice_et2utc, et, 'J', 6, julian
  jul=double(strmid(julian, 2,15))
  CALDAT, jul, MONTH, DAY, YEAR, HOUR, MINUTE, SECOND
  DeltaT=Julday(month,day,year,hour,minute,second)-$
         Julday(1,1,2004,0,0,0)-owlt/86400.
  phasedrift = poly(DeltaT, coefs)
  lambdasun = 360.*(1.0/0.4497)*DeltaT-phasedrift+ 100.0
  lambdasun = lambdasun mod 360. + 360.*(lambdasun lt 0.)
  CassiniLong=(12.0-ltime)*15.+lambdasun
  CassiniLong=CassiniLong mod 360 + 360*(CassiniLong lt 0.)
  SLS3=REFORM(CassiniLong)
  return, sls3

end






;*************************************************
;SLS4
;*************************************************
function caps_ephemeris::sls4,time

  ;The SLS4 longitude system (North) is only valid for this time period:
  ;5 April 2006 to 16 September 2009.
  cspice_utc2et, '2006-111T00:00:00.000',et1
  cspice_utc2et, '2006-111T00:00:00.000',et2
    
  ;The SLS4 longitude system (South) is only valid for this time period:
  ;12 September 2004 to 16 October 2009.
  cspice_utc2et, '2006-111T00:00:00.000',et3
  cspice_utc2et, '2006-111T00:00:00.000',et4     

  print, 'Get SLS4_South...'

  if not self.v_et then *(self.v_et)=self.time_evaluate(time)  
  et=*(self.v_et)

  temp=file_search('/SLS/northsouth.sav')
  nn=n_elements(et)
  if temp eq 0 then return, [[dblarr(nn)],$
                             [dblarr(nn)]]
  stop
  RESTORE, filename='/SLS/northsouth.sav'
  file1='/SLS/south.txt'
  file2='/SLS/north.txt'
  south=READ_ASCII(file1,template=northsouth)
  north=READ_ASCII(file2,template=northsouth)   
  utc_south=strtrim(string(south.year),2)+'-'+$
            strtrim(string(south.day),2)+'T'+$
            strtrim(string(south.hour),2)+':'+$
            strtrim(string(south.minute),2)+':'+$
            strtrim(string(south.second),2)  
  utc_north=strtrim(string(north.year),2)+'-'+$
            strtrim(string(north.day),2)+'T'+$
            strtrim(string(north.hour),2)+':'+$
            strtrim(string(north.minute),2)+':'+$
            strtrim(string(north.second),2)
  cspice_str2et, utc_south, et_south
  cspice_str2et, utc_north, et_north
  SLS4_NORTH=fltarr(n_elements(time))
  SLS4_SOUTH=fltarr(n_elements(time))  

  FOR i=0., n_elements(time)-1 DO BEGIN
     ;------------------------------------------------
     ;SOUTH
     pp_max=where(et_south GT et[i],cc1)
     pp_min=where(et_south LT et[i],cc2)
     IF cc1 EQ 0 OR cc2 EQ 0 THEN SLS4_SOUTH[i]='ffff'x $
     ELSE BEGIN
        tv1=pp_min[n_elements(pp_min)-1]
        tv2=pp_max[0]
        t1=et_south[tv2]-et_south[tv1]
        t=et[i]-et_south[tv1]
        SLS4_SOUTH[i]=(t/t1)*360.
     ENDELSE
     ;----------------------------------------------------
     ;NORTH
     pp_max=where(et_north GT et[i],cc1)
     pp_min=where(et_north LT et[i],cc2)
     IF cc1 EQ 0 OR cc2 EQ 0 THEN SLS4_NORTH[i]='ffff'x $
     ELSE BEGIN
        tv1=pp_min[n_elements(pp_min)-1]
        tv2=pp_max[0]
        t1=et_north[tv2]-et_north[tv1]
        t=et[i]-et_north[tv1]
        SLS4_NORTH[i]=(t/t1)*360.
     ENDELSE
  ENDFOR

  ;Check that dates are within limits
  cspice_utc2et,'2006-095T00:00:00.000',date1_north ;5 April 2006
  cspice_utc2et,'2009-259T00:00:00.000',date2_north ;16 September 2009
  cspice_utc2et,'2004-256T00:00:00.000',date1_south ;12 Septmeber 2004
  cspice_utc2et,'2009-289T00:00:00.000',date2_south ;16 October 2009
  pp1=where(*(self.v_et) le date1_north or *(self.v_et) ge date2_north,cc1)
  pp2=where(*(self.v_et) le date1_south or *(self.v_et) ge date2_south,cc2)

  if cc1 ne 0 then sls4_north[pp1]=65535.D
  if cc2 ne 0 then sls4_south[pp2]=65535.D
;  sls4={north:sls4_north,south:sls4_south}
  sls4=[[sls4_north],[sls4_south]]
  return, sls4

end









;*************************************************
;ACTUATOR
;*************************************************
function caps_ephemeris::actuator,time,act

  if size(act,/type) eq 0 then return, dblarr(n_elements(time))
  if total(act) eq 0 then return, dblarr(n_elements(time))  
  act_time=dblarr(n_elements(act.time)*32.)
  act_angle=dblarr(n_elements(act.time)*32.)
  if not self.v_et then temp=self.time_evaluate(time)  
  j=0.
  FOR i=0., n_elements(act.time)-1 DO BEGIN
     act_time[j:j+31]=act[i].time+findgen(32)
     temp=strsplit(act[i].act_angle,'()',/extract)
     act_angle[j:j+31]=strsplit(temp,',',/extract)
     j=j+32.
  ENDFOR 
  ;Interpolate

  pp=where((act_angle NE -999),cc)
  IF cc NE 0 THEN BEGIN
     angle2=act_angle[pp]
     time2=act_time[pp]
     angle=interpol(angle2, time2, act_time)
  ENDIF

  act_angle=interpol(angle,act_time,*(self.v_et))
  self.v_act=ptr_new(/allocate)
  *(self.v_act)=act_angle
  return, act_angle

end




;*************************************************
;B-Field
;*************************************************
function caps_ephemeris::b_field,time,mag

  nn=dblarr(n_elements(time))
  if size(mag,/type) eq 0 then return, [[nn],[nn],[nn]] 
  if total(mag) eq 0 then return, [[nn],[nn],[nn]] 
  if not self.v_et then temp=self.time_evaluate(time)  
  print, 'Converting mag utc to et...'
  cspice_str2et, reform(mag.utc), et_mag
  ;INTERPOL
  print, 'Calculating Br...'
  br=interpol(mag.b_r,et_mag,*(self.v_et))
  print, 'Calculating Bt...'
  bt=interpol(mag.b_t,et_mag,*(self.v_et))
  print, 'Calculating Bp...'
  bp=interpol(mag.b_p,et_mag,*(self.v_et))  
  print, 'Checking missing values...'    
  ;CHECK MISSING VALUES
  FOR i=1., n_elements(et_mag)-1 DO BEGIN
     temp=et_mag[i]-et_mag[i-1]
     IF temp GT 30. THEN BEGIN
        pp=where((*(self.v_et) GT et_mag[i-1]) AND $
                 (*(self.v_et) LT et_mag[i]),cc)
        IF cc NE 0 THEN BEGIN
           br[pp]='ffff'x
           bt[pp]='ffff'x
           bp[pp]='ffff'x
        ENDIF
     ENDIF
  ENDFOR  

  self.v_b=ptr_new(/allocate)
  *(self.v_b)=[[br],[bt],[bp]]
  return, [[br],[bt],[bp]]
  
end







;*************************************************
;CMAT
;*************************************************
function caps_ephemeris::cmat,time


  if not self.v_et then temp=self.time_evaluate(time)  
  et=*(self.v_et)
                                ;LOAD SPECIFIC KERNELS
  temp=FILE_SEARCH('/kernels/Cassini/ck/*ra.bc')
  tn1='20'+strmid(temp,20,5)
  tn2='20'+strmid(temp,26,5)
  cspice_et2utc,*(self.v_et),'isod',6,utc
  _year=strmid(utc[0],0,4)
  _day=strmid(utc[0],5,3)
  pp1=where((_year+_day GT tn1) AND (_year+_day LT tn2),cc1)
  pp2=where(_year+_day EQ tn2,cc2)
  IF cc1 NE 0 THEN n_temp=temp[pp1[0]-1:pp1[0]+1]
  IF cc2 NE 0 THEN n_temp=temp[pp2[0]-1:pp2[0]+1]
  FOR i=0, n_elements(n_temp)-1 DO cspice_furnsh,n_temp[i]
  SC_name = 'Cassini'
  SC_ID   = -82
  SC_Bus = -82000
  tol_sec=5.
  cspice_sce2s,SC_ID,tol_sec,temp1
  cspice_sce2s,SC_ID,0.,temp2
  cspice_scencd,SC_ID,temp1,temp11
  cspice_scencd,SC_ID,temp2,temp22
  toltik=temp11-temp22    
  print, 'Calculating CMAT...'
  cmat_tot=dblarr(3,3,n_elements(time))

  FOR i=0., n_elements(time)-1 DO BEGIN
     cspice_sce2c, SC_ID, et[i], sclkdp
     cspice_ckgpav,SC_Bus,sclkdp,toltik,'J2000',cmat, av, clkout, found
     cmat_tot[*,*,i]=transpose(cmat)
  ENDFOR

  self.v_cmat=ptr_new(/allocate)
  *(self.v_cmat)=cmat_tot
  return, cmat_tot
end





;*************************************************
;J2000 to RTP
;*************************************************
function caps_ephemeris::j2000_to_rtp,time


  if not self.v_j2000 then temp=self.j2000(time)  
  j2000=*(self.v_j2000)
  J2000_TO_RTP=dblarr(3,3,n_elements(time))
  Saturn_DEC_J2000=83.537544*!DTOR
  SATURN_RA_J2000=40.583441*!DTOR
  OJ2000=[COS(Saturn_DEC_J2000)*COS(SATURN_RA_J2000), $
          COS(Saturn_DEC_J2000)*SIN(SATURN_RA_J2000),$
          SIN(Saturn_DEC_J2000)]
  for i_state=0, n_elements(time)-1 do begin
     cspice_vhat, reform(j2000.state1[0:2,i_state]), RJ2000
     cspice_vcrss, OJ2000,RJ2000,temp1
     cspice_vhat, temp1, temp2
     cspice_vcrss, temp2,RJ2000, TJ2000_temp
     cspice_vhat,TJ2000_temp, TJ2000
     cspice_vcrss,RJ2000,TJ2000,PJ2000_temp
     cspice_vhat, PJ2000_temp, PJ2000
     J2000_TO_RTP[*,*,i_state]=[[RJ2000],[TJ2000],[PJ2000]]
  endfor

  self.v_j2000_to_rtp=ptr_new(/allocate)
  *(self.v_j2000_to_rtp)=J2000_TO_RTP 
  return,J2000_TO_RTP 

end





;*************************************************
;RTP to MAG
;*************************************************
function caps_ephemeris::rtp_to_mag,time,mag

  if size(mag,/type) eq 0 then return, dblarr(n_elements(time))
  if total(mag) eq 0 then return, dblarr(n_elements(time))
  rtp_to_mag=dblarr(3,3,n_elements(time))
  if not self.v_b then temp=self.b_field(time,mag)  
  bb=*(self.v_b)  
  for i_state=0, n_elements(time)-1 do begin
     b=[bb[i_state,0],bb[i_state,1],bb[i_state,2]]
     cspice_vhat,b,b_II
     cspice_vcrss,b_II,[0,0,1],b_perp1
     cspice_vhat,b_perp1,b_perp11
     cspice_vcrss,b_perp11,b_II,b_perp2
     cspice_vhat, b_perp2,b_perp22    
     RTP_TO_MAG[*,*,i_state]=[[b_perp11],[b_perp22],[b_II]]
  endfor

  self.v_rtp_to_mag=ptr_new(/allocate)
  *(self.v_rtp_to_mag)=rtp_to_mag 
  return, rtp_to_mag

end




;*************************************************
;SC to J2000
;*************************************************
function caps_ephemeris::sc_to_j2000,time

  if not self.v_cmat then temp=self.cmat(time)  
 
  self.v_sc_to_j2000=ptr_new(/allocate)
  self.v_sc_to_j2000=self.v_cmat  

  return, *(self.v_cmat)

end




;*************************************************
;SC to RTP Velocity Vector
;*************************************************
function caps_ephemeris::sc_rtp_vel,time,act

  sc_rtp_vel=dblarr(3,n_elements(time))

  if not self.v_j2000 then temp=self.j2000(time)  
  if not self.v_j2000_to_rtp then temp=self.j2000_to_rtp(time,act)  

  j2000=*(self.v_j2000)
  j2000_to_rtp=*(self.v_j2000_to_rtp)

  for i_state=0., n_elements(time)-1 do begin
     SC_RTP_VEL[*,i_state]=reform(J2000_TO_RTP[*,*,i_state]) ## $
                           j2000.state1[3:5,i_state]
  endfor

  self.v_sc_rtp_vel=ptr_new(/allocate)
  *(self.v_sc_rtp_vel)=sc_rtp_vel  
  return, sc_rtp_vel

end





;*************************************************
;SC Look Vector
;*************************************************
function caps_ephemeris::sc_look,time,act


  if size(act,/type) eq 0 then return, dblarr(n_elements(time))
  if total(act) eq 0 then return, dblarr(n_elements(time))
  sc_look=dblarr(3,8,n_elements(time))
  if not self.v_act then temp=self.actuator(time,act)
  P=[70.,50.,30.,10.,-10.,-30.,-50.,-70.]*!DTOR
  ;P=[80.,60.,40.,20.,0.,-20.,-40.,-60.]*!DTOR
  ;P=[60.,40.,20.,0.,-20.,-40.,-60.,-80.]*!DTOR
  A=*(self.v_act)*!DTOR
  for i=0., 7 do begin
     for i_state=0., n_elements(time)-1 do begin
        sc_look[*,i,i_state]=[-1.*COS(P[i])*SIN(A[i_state]),$
                              -1.*COS(P[i])*COS(A[i_state]),$
                              SIN(P[i])]
     endfor
  endfor

  self.v_sc_look=ptr_new(/allocate)
  *(self.v_sc_look)=sc_look  

  return, sc_look

end




;*************************************************
;SC Look Vector
;*************************************************
function caps_ephemeris::sc_rtp_look,time,act


  nn=dblarr(n_elements(time))
  if size(act,/type) eq 0 then return, dblarr(3,8)
  if total(act) eq 0 then return, dblarr(3,8)

  sc_rtp_look=dblarr(3,8,n_elements(time))
  if not self.v_sc_look then temp=self.sc_look(time,act)  
  if not self.v_sc_to_j2000 then temp=self.sc_to_j2000(time)  
  if not self.v_j2000_to_rtp then temp=self.j2000_to_rtp(time)  
  sc_look=*(self.v_sc_look)
  sc_to_j2000=*(self.v_sc_to_j2000)
  j2000_to_rtp=*(self.v_j2000_to_rtp)

  for i=0., 7 do begin
     for i_state=0., n_elements(time)-1 do begin
        cspice_vhat,reform(SC_LOOK[*,i,i_state]),sc_look_temp
        temp3=SC_TO_J2000[*,*,i_state] ## sc_look_temp
        SC_RTP_LOOK[*,i,i_state]=J2000_TO_RTP[*,*,i_state] ## temp3
     endfor
  endfor
  self.v_sc_rtp_look=ptr_new(/allocate)
  *(self.v_sc_rtp_look)=sc_rtp_look  
  return, sc_rtp_look

end


;*************************************************
;Pitch Angles
;*************************************************
function caps_ephemeris::pitch_angle,time,mag,act

  if size(act,/type) eq 0 then return, dblarr(8)
  if total(act) eq 0 then return, dblarr(8)
  pitch_angle=dblarr(8,n_elements(time))
  if not self.v_rtp_to_mag then temp=self.rtp_to_mag(time,mag)
  if not self.v_sc_rtp_look then temp=self.sc_rtp_look(time,act)
  rtp_to_mag=*(self.v_rtp_to_mag)
  sc_rtp_look=*(self.v_sc_rtp_look)

  for i_state=0., n_elements(time)-1 do begin
     for i=0., 7 do begin
        temp4=reform(RTP_TO_MAG[*,*,i_state]) ## $
              (-1.*reform(SC_RTP_LOOK[*,i,i_state]))
        pitch_angle[i,i_state]=ACOS(temp4[2])
     endfor
  endfor
  
  self.v_pitch_angle=ptr_new(/allocate)
  *(self.v_pitch_angle)=pitch_angle  
  return, pitch_angle

end




;*************************************************
;Mag Angles
;*************************************************
function caps_ephemeris::mag_angle,time,mag,act

  nn=dblarr(n_elements(time))
  if size(act,/type) eq 0 then return, dblarr(3,8)
  if total(act) eq 0 then return, dblarr(3,8)
  mag_angle=dblarr(3,8,n_elements(time))
  if not self.v_rtp_to_mag then temp=self.rtp_to_mag(time,mag)
  if not self.v_sc_rtp_look then temp=self.sc_rtp_look(time,act)
  rtp_to_mag=*(self.v_rtp_to_mag)
  sc_rtp_look=*(self.v_sc_rtp_look)

  for i_state=0., n_elements(time)-1 do begin
     for i=0., 7 do $
        mag_angle[*,i,i_state]=$
           reform(RTP_TO_MAG[*,*,i_state]) ## $
           (-1.*reform(SC_RTP_LOOK[*,i,i_state]))        
  endfor


  self.v_mag_angle=ptr_new(/allocate)
  *(self.v_mag_angle)=mag_angle  
  return, mag_angle

end




;      corot_angle[i,i_state]=cspice_vsep(reform(temp4),[0,1,0])



;*************************************************
;SC OAS Flow
;*************************************************
function caps_ephemeris::sc_oas_flow,time,act

  if size(act,/type) eq 0 then return, dblarr(3,8)
  if total(act) eq 0 then return, dblarr(3,8)
  sc_oas_flow=dblarr(3,8,n_elements(time))
  if not self.v_sc_rtp_look then temp=self.sc_rtp_look(time,act)
  sc_rtp_look=*(self.v_sc_rtp_look)

  for i=0., 7 do begin
     for i_state=0., n_elements(time)-1 do begin
        sc_oas_flow_t=[-1.*sc_rtp_look[1,i,i_state],$
                       sc_rtp_look[2,i,i_state],$
                       -1.*sc_rtp_look[0,i,i_state]]
        cspice_vhat,sc_oas_flow_t, sc_oas_flow_m
        sc_oas_flow[*,i,i_state]=sc_oas_flow_m
     endfor
  endfor

  self.v_sc_oas_flow=ptr_new(/allocate)
  *(self.v_sc_oas_flow)=sc_oas_flow  
  return, sc_oas_flow
end





;*************************************************
;OAS Angles
;*************************************************
function caps_ephemeris::oas_angle,time,act

  vec={theta_oas:dblarr(8),phi_oas:dblarr(8)}
  if size(act,/type) eq 0 then return, vec
  if total(act) eq 0 then return, vec
  theta_oas=dblarr(8,n_elements(time))
  phi_oas=dblarr(8,n_elements(time))

  if not self.v_sc_oas_flow then temp=self.sc_oas_flow(time,act)
  sc_oas_flow=*(self.v_sc_oas_flow)

  for i_state=0., n_elements(time)-1 do begin
     for i=0., 7 do begin
        THETA_OAS[i,i_state]=ACOS(SC_OAS_FLOW[2,i,i_state])
        PHI_OAS[i,i_state]=ATAN(SC_OAS_FLOW[1,i,i_state],$
                                SC_OAS_FLOW[0,i,i_state])
     endfor
  endfor

  self.v_oas_angle=ptr_new(/allocate)
  *(self.v_oas_angle)={theta_oas:theta_oas, phi_oas:phi_oas}
  return, {theta_oas:theta_oas, phi_oas:phi_oas}

end




;*************************************************
;Spacecract Velocity RTP
;*************************************************
function caps_ephemeris::sc_rtp_vel,time,act

  if size(act,/type) eq 0 then return, dblarr(3)
  if total(act) eq 0 then return, dblarr(3)
  if not self.v_j2000 then temp=self.j2000(time) 
  j2000=*(self.v_j2000)
  if not self.v_sc_rtp_look then temp=self.j2000_to_rtp(time,act)
  j2000_to_rtp=*(self.v_j2000_to_rtp)
  nn=n_elements(time)
  sc_rtp_vel=dblarr(3,nn)

  for i=0., n_elements(time)-1 do begin
     sc_rtp_vel[*,i]=reform(j2000_to_rtp[*,*,i]) ## j2000.state1[3:5,i]
  endfor
  
  self.v_sc_rtp_vel=ptr_new(/allocate)
  *(self.v_sc_rtp_vel)=sc_rtp_vel
  return, sc_rtp_vel

end





;*************************************************
;Create Ephemeris Structure
;*************************************************
function caps_ephemeris::create_vars,time,mag,act

  et=self.time_evaluate(time)
  cspice_et2utc,et,'isod',6,utc
  r=self.radial(time)
  lshell=self.lshell(time)
  lat=self.colatitude(time)
  ltime=self.localtime(time)
  equ=self.equ(time)
  ksm=self.ksm(time)
  sls3=self.sls3(time)
  sls4=self.sls4(time)
  act=self.actuator(time,act)
  b=self.b_field(time,mag)
  sc_rtp_look=self.sc_rtp_look(time,act)
  sc_oas_flow=self.sc_oas_flow(time,act)
  sc_rtp_vel=self.sc_rtp_vel(time,act)
  pitch_angle=self.pitch_angle(time,mag,act)
  oas_angle=self.oas_angle(time,act)
  mag_angle=self.mag_angle(time,mag,act)


  vars=replicate($
       {et:0.D,utc:'',r:0.D,lshell:0.D,$
        lat:0.D,ltime:0.D,equ:dblarr(3),$
        ksm:dblarr(3),sls3:0.D,$
        sls4:dblarr(2),b:dblarr(3),$
        act:0.D,$
        sc_rtp_vel:dblarr(3),$
        sc_rtp_look:dblarr(3,8),$
        sc_oas_flow:dblarr(3,8),$
        pitch_angle:dblarr(8),$
        mag_angle:dblarr(3,8),$
        oas_phi:dblarr(8),$
        oas_theta:dblarr(8),$
        oas_angle:dblarr(3,8)},$
       n_elements(time))

  vars.et=et
  vars.utc=utc
  vars.r=r
  vars.lshell=lshell
  vars.lat=lat
  vars.ltime=ltime
  vars.equ=transpose(equ)
  vars.ksm=transpose(ksm)
  vars.sls3=sls3
  vars.sls4=transpose(sls4)
  vars.act=act
  vars.b=transpose(b)
  vars.sc_rtp_look=sc_rtp_look
  vars.sc_oas_flow=sc_oas_flow
  vars.sc_rtp_vel=sc_rtp_vel
  vars.pitch_angle=pitch_angle
  vars.mag_angle=mag_angle
  vars.oas_theta=oas_angle.theta_oas
  vars.oas_phi=oas_angle.phi_oas
;  vars={et:et,$
;        utc:utc,$
;        r:r,$
;        lshell:lshell,$
;        lat:lat,$
;        ltime:ltime,$
;        equ:equ,$
;        ksm:ksm,$
;        sls3:sls3,$
;        sls4:sls4,$
;        act:act,$
;        b:b,$
;        sc_rtp_look:sc_rtp_look,$
;        sc_oas_flow:sc_oas_flow,$
;        pitch_angle:pitch_angle,$
;        mag_angle:mag_angle,$
;        oas_angle:oas_angle}
  return,vars

end


function caps_ephemeris2::create_vars_moons,moon,time,mag,act

  temp=self.target(moon)
  et=self.time_evaluate(time)
  cspice_et2utc,et,'isod',6,utc
  r=self.radial(time)
  lshell=self.lshell(time)
  lat=self.colatitude(time)
  ltime=self.localtime(time)
  equ=self.equ(time)
  ksm=self.ksm(time)
  sls3=self.sls3(time)
  sls4=self.sls4(time)
  act=self.actuator(time,act)
  b=self.b_field(time,mag)
  sc_rtp_look=self.sc_rtp_look(time,act)
  sc_oas_flow=self.sc_oas_flow(time,act)
  sc_rtp_vel=self.sc_rtp_vel(time,act)
  pitch_angle=self.pitch_angle(time,mag,act)
  oas_angle=self.oas_angle(time,act)
  mag_angle=self.mag_angle(time,mag,act)


  vars=replicate($
       {et:0.D,utc:'',r:0.D,lshell:0.D,$
        lat:0.D,ltime:0.D,equ:dblarr(3),$
        ksm:dblarr(3),sls3:0.D,$
        sls4:dblarr(2),b:dblarr(3),$
        act:0.D,$
        sc_rtp_vel:dblarr(3),$
        sc_rtp_look:dblarr(3,8),$
        sc_oas_flow:dblarr(3,8),$
        pitch_angle:dblarr(8),$
        mag_angle:dblarr(3,8),$
        oas_phi:dblarr(8),$
        oas_theta:dblarr(8),$
        oas_angle:dblarr(3,8)},$
       n_elements(time))


  vars.et=et
  vars.utc=utc
  vars.r=r
  vars.lshell=lshell
  vars.lat=lat
  vars.ltime=ltime
  vars.equ=transpose(equ)
  vars.ksm=transpose(ksm)
  vars.sls3=sls3
  vars.sls4=transpose(sls4)
  vars.act=act
  vars.b=transpose(b)
  vars.sc_rtp_look=sc_rtp_look
  vars.sc_oas_flow=sc_oas_flow
  vars.sc_rtp_vel=sc_rtp_vel
  vars.pitch_angle=pitch_angle
  vars.mag_angle=mag_angle
  vars.oas_theta=oas_angle.theta_oas
  vars.oas_phi=oas_angle.phi_oas
  return,vars
end




;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------
;--------------------------------------------------

pro caps_ephemeris__define

  void={caps_ephemeris,$

        target_name:ptr_new(),$
        SC_name:ptr_new(),$
        SC_ID:ptr_new(),$
        SC_Bus:ptr_new(),$

        tol_sec:ptr_new(),$

        v_et:ptr_new(),$
        v_r:ptr_new(),$
        v_ltime:ptr_new(),$
        v_lat:ptr_new(),$
        v_lshell:ptr_new(),$
        v_ksm:ptr_new(),$
        v_ksm_vel:ptr_new(),$
        v_equ:ptr_new(),$
        v_equ_vel:ptr_new(),$
        v_sls3:ptr_new(),$
        v_sls4:ptr_new(),$
        v_act:ptr_new(),$
        v_b:ptr_new(),$
        v_mag_angle:ptr_new(),$
        v_cmat:ptr_new(),$
        v_j2000:ptr_new(),$
        v_sc_rtp_vel:ptr_new(),$
        v_sc_to_j2000:ptr_new(),$
        v_sc_look:ptr_new(),$
        v_sc_rtp_look:ptr_new(),$
        v_sc_oas_flow:ptr_new(),$
        v_j2000_to_rtp:ptr_new(),$
        v_rtp_to_mag:ptr_new(),$
        v_pitch_angle:ptr_new(),$
        v_oas_angle:ptr_new(),$
        inherits mysql}
;        inherits caps}
  return

end



