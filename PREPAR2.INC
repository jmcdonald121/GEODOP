       CHARACTER*4   IRJT , IBLK , IRJV , NEWFT, ISRC
       CHARACTER*10  NAME , ISTN
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , NTIM , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA
       COMMON /DEV/  IEPH , INPT , IOUT , IRED , IPRT
       COMMON /TIM/     A ,    B , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
       COMMON/DAT/  IFRC, MAXD
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
       DIMENSION GXYZ( 3) , EXPT(99) ,   A(3,2) , B(3,3,3) , MESS( 3)
     +         ,   A1( 3) ,   A2( 3) ,  B1(3,3) ,  B2(3,3) ,  B3(3,3)
       DIMENSION APRI(3,2), LDOP(10) , TAUT(10) ,   V( 3)  , INTA(30)
       DIMENSION  IRJCT(50)
       EQUIVALENCE (A(1,1),A1(1)) , (A(1,2),A2(1)),(B(1,1,3),B3(1,1))
     +           , (B(1,1,1),B1(1,1)) ,(B(1,1,2),B2(1,1))
       DATA APRI / 4.4444444D-07 , 1.7777777D-04 , 2.7777777D-04
     +           , 4.4444444D-07 , 1.1111111D-01 , 1.1111111D-01 /
     +                             , DAYS / 6.9444444D-04 /
     +    , IBLK /  '    '/ , IRJT /  'RJ99' /
     +    , SCHI / 13.4D-03 / , SIZE / 10.D0 /
     +    , STOM / 1.6666667D-02 / , TMAX / 1.0000000D-05 /
     +    , XMTM / 6.0000000D+04 / , XNIN / 1.1111111D-01 /
     +    , XSEC / 4.6296296D+00 / , XTIR / 1.1574074D-08 /
     +    , IRJU /  'RJDF'       /
     +    , IRJV /  'RJ3S'       / , TONE / 5.0000000D-04 /
     +      , S0SQ /1.111D-03/, TERR/1.0D-02/
     +  ,TOFF/.0129239D0/,NEWFT/ 'NEWF' /

