temp=strtrim(sindgen(60),2)
temp[0:9]='0'+temp[0:9]

utc=strarr(24.*60.*60.)
for i=0., 23. do $
for j=0., 59. do $
for k=0., 59. do $
   utc[(i)*3600.+j*60.+k]='2009-141T'+temp[i]+':'+temp[j]+':'+temp[k]


dbuser="root"
dbpass=""
dbloc="jdbc:mysql://localhost:3306/MAG_KRTP_LEVEL1"
oMySql = obj_new('MySql', dbloc, dbuser, dbpass)
_year='2009'
_day='141'
cmd="SELECT utc,b_r,b_t,b_p FROM `09137_09153` "+$
    "WHERE year="+_year+" AND day="+_day+";"
print, 'Load MAG Data'
mag=oMySql->query(cmd)

obj_destroy, oMySql
dbloc="jdbc:mysql://localhost:3306/ACT_LEVEL0"
oMySql  = obj_new('MySql', dbloc, dbuser, dbpass)
cmd="SELECT * FROM `"+_year+_day+"`;"
print, 'Load Actuator Data'
act=oMySql->QUERY(cmd)
obj_destroy, oMySql



;------------------------------------
;Tests


eph=obj_new('caps_ephemeris2')



all=eph->create_vars_moons('Titan',utc,mag,act)

;-----------------------------
;Local Time
;ltime=eph->localtime(utc)
;stop
;-----------------------------
;Latitude
;lat=eph->colatitude(utc)
;stop
;-----------------------------
;R (km)
;r=eph->radial(utc)
;stop
;-----------------------------
;L-Shell
;lshell=eph->lshell(utc)
;stop

;-----------------------------
;Equatorial
;equ=eph->equ(utc)
;stop
;-----------------------------
;KSM
;ksm=eph->ksm(utc)
;stop

;-----------------------------
;SLS3
;sls3=eph->sls3(utc)
;stop
;-----------------------------
;SLS4
;sls4=eph->sls4(utc)
;stop
;-----------------------------
;Actuator
;actt=eph->actuator(utc,act)
;stop
;-----------------------------
;B-Field
;b=eph->b_field(utc,mag)
;stop
;-----------------------------
;CMAT
;cmat=eph->cmat(utc)
;stop
;-----------------------------
;J2000 to RTP
;j2000_to_rtp=eph->j2000_to_rtp(utc)
;stop
;-----------------------------
;RTP to MAG
;rtp_to_mag=eph->rtp_to_mag(utc,mag)
;stop
;-----------------------------
;SC to J2000
;sc_to_j2000=eph->sc_to_j2000(utc)
;stop
;-----------------------------
;SC to RTP Velocity
;sc_rtp_vel=eph->sc_rtp_vel(utc,act)
;stop
;-----------------------------
;SC Look Vector
;sc_look=eph->sc_look(utc,act)
;stop
;-----------------------------
;SC RTP Look Vector
;sc_rtp_look=eph->sc_rtp_look(utc,act)
;stop
;-----------------------------
;Pitch Angle
;pitch_angle=eph->pitch_angle(utc,mag,act)
;stop
;-----------------------------
;SC OAS Flow
;sc_oas_flow=eph->sc_oas_flow(utc,act)
;stop
;-----------------------------
;OAS Angles
oas_angles=eph->oas_angle(utc,act)
;stop


;temp=mysql_pitch_angle(utc)
;stop


;***************************************
;Comparison


;--------------
;Iowa
iowa_r=13.028
iowa_lat=-0.387
iowa_L=13.0
sls3=326.127
sls4_south=10
sls4_north=10

iowa_ksm_x=11.411
iowa_ksm_y=-4.821
iowa_ksm_z= 4.035

iowa_ksm_x_vel=-3.868
iowa_ksm_y_vel= 7.673
iowa_ksm_z_vel=-1.356

iowa_equ_x=12.103
iowa_equ_y=-4.821
iowa_equ_z=-0.088

iowa_equ_x_vel=-4.099
iowa_equ_y_vel= 7.673
iowa_equ_z_vel= 0.040


iowa_var=$
   'DOY       TOP_O          TOP_A          TOP_S          TOP_R          TOP_THETA      TOP_PHI        1/2_O          1/2_A          1/2_S          1/2_R          1/2_THETA      1/2_PHI        2/3_O          2/3_A          2/3_S          2/3_R          2/3_THETA      2/3_PHI        3/4_O          3/4_A          3/4_S          3/4_R          3/4_THETA      3/4_PHI        4/5_O          4/5_A          4/5_S          4/5_R          4/5_THETA      4/5_PHI        5/6_O          5/6_A          5/6_S          5/6_R          5/6_THETA      5/6_PHI        6/7_O          6/7_A          6/7_S          6/7_R          6/7_THETA      6/7_PHI        7/8_O          7/8_A          7/8_S          7/8_R          7/8_THETA      7/8_PHI        BOT_O          BOT_A          BOT_S          BOT_R          BOT_THETA      BOT_PHI        SUN_O          SUN_A          SUN_S          SUN_R          SUN_THETA      SUN_PHI        SATURN_R       V_O            V_A            V_S            V_R            V_THETA        V_PHI          COROT_O        COROT_A        COROT_S        COROT_R        COROT_THETA    COROT_PH'

iowa_num=$
   '302.00000 -6.6139068E-01 -7.4189854E-01 -1.1022216E-01  1.0000000E+00  1.6812429E+00 -2.2988865E+00 -8.4127841E-01 -4.7953857E-01 -2.4958646E-01  1.0000000E+00  1.8230495E+00 -2.6235153E+00 -9.1969554E-01 -1.5933917E-01 -3.5884695E-01  1.0000000E-00  1.9378286E+00 -2.9700435E+00 -8.8718383E-01  1.8007888E-01 -4.2482521E-01  1.0000000E+00  2.0095651E+00  2.9413352E+00 -7.4766465E-01  4.9777676E-01 -4.3956327E-01  1.0000000E+00  2.0259087E+00  2.5542073E+00 -5.1796608E-01  7.5543542E-01 -4.0128352E-01  1.0000000E+00  1.9837140E+00  2.1718280E+00 -2.2579316E-01  9.2197742E-01 -3.1460305E-01  1.0000000E+00  1.8908348E+00  1.8109702E+00  9.3613747E-02  9.7731534E-01 -1.8997681E-01  1.0000000E+00  1.7619349E+00  1.4753010E+00  4.0172945E-01  9.1477461E-01 -4.2436568E-02  1.0000000E+00  1.6132456E+00  1.1569961E+00 -4.5496239E+08  4.7350387E+08 -1.1911655E+09  1.3601735E+09  2.6377737E+00  2.3362271E+00  7.8546092E+05 -4.3613758E-03  5.6128305E+00  6.6470286E+00  8.6998204E+00  7.0124111E-01  1.5715734E+00  4.3613758E-03  1.2176247E+02 -6.6470286E+00  1.2194377E+02  1.6253323E+00  1.5707605E+00'

iowa_num=$
'291.00000 -8.7457738E-01  1.2890641E-01  4.6743721E-01  1.0000000E+00  1.0844068E+00  2.9952535E+00 -9.0207991E-01  3.7119784E-01  2.2014541E-01  1.0000000E-00  1.3488328E+00  2.7512196E+00 -8.2077829E-01  5.6871733E-01 -5.3699169E-02  1.0000000E-00  1.6245213E+00  2.5356477E+00 -6.4047870E-01  6.9764111E-01 -3.2106684E-01  1.0000000E-00  1.8976521E+00  2.3135020E+00 -3.8292792E-01  7.4241909E-01 -5.4970911E-01  1.0000000E-00  2.1528123E+00  2.0469913E+00 -7.9190391E-02  6.9765036E-01 -7.1204835E-01  1.0000000E-00  2.3632076E+00  1.6838227E+00  2.3409867E-01  5.6873471E-01 -7.8850405E-01  1.0000000E-00  2.4791692E+00  1.1803189E+00  5.1915198E-01  3.7122126E-01 -7.6985453E-01  1.0000000E-00  2.4494095E+00  6.2075747E-01  7.4158790E-01  1.2893305E-01 -6.5834919E-01  1.0000000E-00  2.2894198E+00  1.7214008E-01  6.0021335E+08 -7.7937889E+08  9.9367913E+08  1.3982438E+09  7.8035720E-01 -9.1454382E-01  2.7214908E+05 -1.3455506E+01  4.7491923E+00  4.7963932E+00  1.5053600E+01  1.2465220E+00  2.8022875E+00  1.3455506E+01  3.0641841E+01 -4.7963932E+00  3.3807964E+01  1.7131483E+00  1.1570253E+00'

iowa_num=$
'291.10347  8.0480292E-02  5.4083609E-01 -8.3726892E-01  1.0000000E-00  2.5630655E+00  1.4230732E+00  3.5874421E-01  6.5585586E-01 -6.6419552E-01  1.0000000E-00  2.2972135E+00  1.0702698E+00  5.9373828E-01  6.9176973E-01 -4.1101033E-01  1.0000000E+00  1.9943584E+00  8.6150977E-01  7.5711875E-01  6.4424597E-01 -1.0825124E-01  1.0000000E-00  1.6792601E+00  7.0502677E-01  8.2917953E-01  5.1901663E-01  2.0756455E-01  1.0000000E-00  1.3617117E+00  5.5927498E-01  8.0122902E-01  3.3118623E-01  4.9834500E-01  1.0000000E+00  1.0491075E+00  3.9195986E-01  6.7663847E-01  1.0340989E-01  7.2901768E-01  1.0000000E+00  7.5391058E-01  1.5165541E-01  4.7043533E-01 -1.3683922E-01  8.7176007E-01  1.0000000E-00  5.1201293E-01 -2.8306698E-01  2.0749075E-01 -3.6058349E-01  9.0935534E-01  1.0000000E-00  4.2906449E-01 -1.0486385E+00  4.8414956E+07 -5.6927873E+08  1.2762253E+09  1.3982751E+09  4.2091853E-01 -1.4859543E+00  2.4134370E+05 -1.5502460E+01  4.3287718E+00  1.8138233E+00  1.6197360E+01  1.4585783E+00  2.8692969E+00  1.5502460E+01  3.4500147E+01 -1.8138233E+00  3.7866560E+01  1.6187151E+00  1.1484874E+00'

iowa_vars=strsplit(iowa_var,/extract)
iowa_nums=double(strsplit(iowa_num,/extract))

print, iowa_vars[65:67]
print, iowa_nums[65:67]
print, all[0].sc_rtp_vel[*]

;print, iowa_vars[7:9]
;print, iowa_nums[7:9]
;print, sc_oas_flow[*,1,0]

;print, iowa_vars[13:15]
;print, iowa_nums[13:15]
;print, sc_oas_flow[*,2,0]


device, decomposed=0
cgloadct,39
ang=90.
range=indgen(15*60);+15*60




phi=oas_angles.phi_oas
theta=oas_angles.theta_oas
;phi=!DTOR*(((phi*!RADEG+ang mod 360)));+360.)
phi=!DTOR*(((-1.*(phi*!RADEG)+360.)+90.) mod 360)
aa0=phi[0,range]
rr0=theta[0,range]*!RADEG  
plot, rr0*COS(aa0),rr0*SIN(aa0),$
      ystyle=5,$
      xstyle=5,$
      yrange=[-180,180],$
      xrange=[-180,180],/iso,psym=1
for i=1, 7 do begin
   aa=phi[i,range]
   rr=theta[i,range]*!RADEG  
   oplot, rr*COS(aa),$
          rr*SIN(aa),$
          color=i*30.,psym=1
endfor

oplot, [0,0],[0,0],color=250,symsize=1,psym=2,thick=1
yy=90*SIN(indgen(360)*!DTOR)
xx=90*COS(indgen(360)*!DTOR)
oplot,xx,yy,color=250,linestyle=2 

yy=180*SIN(indgen(360)*!DTOR)
xx=180*COS(indgen(360)*!DTOR)
oplot,xx,yy,linestyle=2 


stop

end
