!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      PROGRAM SINV
 
      PARAMETER (MAXA=16000000)
      PARAMETER (MAXS=1000)
 
      CHARACTER(255) FILE   
      CHARACTER(8)  SUBSET
      CHARACTER     ci*16,cj*80
      DIMENSION     isat(0:maxs,0:maxs)  
      real(8)       arr(2,maxa),said(maxa),siid(maxa)
 
      DATA BMISS  /10E10/
      DATA LUNBF  /20/
 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 
      isat=0  
      jsat=0  
      said=0
      ssid=0

!  get filename argument

      open(6,recl=130)
      NARG=IARGC()
      IF(NARG/=1) THEN
        write(6,*)'Usage: sinv <satbufrfile> will print inventory of satellites by platform and instrument'
        CALL EXIT(2)
      ENDIF
      call getarg(1,file)
      file = TRIM(file)//CHAR(0)
      open(lunbf,file=file,form='unformatted')
 
      CALL OPENBF(LUNBF,'IN',LUNBF)

      call ufbtab(lunbf,said,1,maxa,nret,'SAID')
      !print*,nret
      call ufbtab(lunbf,siid,1,maxa,nrex,'SIID')
      !print*,nrex

      do n=1,max(nret,nrex)
      i = said(n)
      j = siid(n)  
      if(i>maxs.or.i<0) i=0                 
      if(j>maxs.or.j<0) j=0                 
      isat(i,j) = isat(i,j)+1
      enddo

      !print1,'satellite     ','instrument    ','   count'
      !print'(40("-"))'
      print*
1     format(a14,2x,a14,2x,a8)
      do i=0,1000
      do j=0,1000
      if(isat(i,j).gt.0) then 
         jsat=jsat+isat(i,j)
         call satcode(i,ci,j,cj)
         print'(i3.3,2x,a16,2x,i10,2x,i3.3,a80)',i,ci,isat(i,j),j,trim(cj)
      endif
      enddo
      enddo; print'(23x,i10)',jsat; print*

      stop
      end
!-----------------------------------------------------------------------
!  looks up BUFR code table values for SAID (said) and SIID (instrument)
!-----------------------------------------------------------------------
      subroutine satcode(icode,csad,jcode,csid)

      common /satlines/ sadline(1000),sidline(1000)
      character(80)     sadline,sidline

      character(16) csad,saic(1000)
      character(80) csid,siic(1000)
      integer said,siid
      logical first /.true./

      csad=' '; csid=' '

      if(first)then
         do i=1,1000
         read(sadline(i),*,err=2) said,saic(said)
         !write(6,*)i,said,saic(said)
         enddo
2        do i=1,350 
         read(sidline(i),'(i3,a80)',err=3) siid,siic(siid)
         !write(6,*)i,siid,siic(siid)
         enddo
3        first=.false.
4        format(i3,a80)
      endif

!  figure out what satellite this really is

      if(icode>0) csad=saic(icode)
      if(jcode>0) csid=siic(jcode)

      return
      end
!-----------------------------------------------------------------------
!  looks up BUFR code table values for SAID (said) and SIID (instrument)
!-----------------------------------------------------------------------
block data

common /satlines/ sadline(1000),sidline(1000)
character(80)     sadline,sidline

data sadline(1) /'001 ERS-1'/
data sadline(2) /'002 ERS-2'/
data sadline(3) /'003 METOP-B'/
data sadline(4) /'004 METOP-A'/
data sadline(5) /'005 METOP-C'/
data sadline(6) /'20 SPOT-1'/
data sadline(7) /'21 SPOT-2'/
data sadline(8) /'22 SPOT-3'/
data sadline(9) /'23 SPOT-4'/
data sadline(10) /'40 OERSTED'/
data sadline(11) /'41 CHAMP'/
data sadline(12) /'42 TerraSAR-X'/
data sadline(13) /'43 TanDEM-X'/
data sadline(14) /'44 PAZ'/
data sadline(15) /'46 SMOS'/
data sadline(16) /'47 CryoSat-2'/
data sadline(17) /'48 AEOLUS'/
data sadline(18) /'50 METEOSAT-3'/
data sadline(19) /'51 METEOSAT-4'/
data sadline(20) /'52 METEOSAT-5'/
data sadline(21) /'53 METEOSAT-6'/
data sadline(22) /'54 METEOSAT-7'/
data sadline(23) /'55 METEOSAT-8'/
data sadline(24) /'56 METEOSAT-9'/
data sadline(25) /'57 METEOSAT-10'/
data sadline(26) /'58 METEOSAT-1'/
data sadline(27) /'59 METEOSAT-2'/
data sadline(28) /'60 ENVISAT'/
data sadline(29) /'61 Sentinel-3A'/
data sadline(30) /'62 Sentinel-1A'/
data sadline(31) /'63 Sentinel-1B'/
data sadline(32) /'64 Sentinel-5P'/
data sadline(33) /'65 Sentinel-3B'/
data sadline(34) /'66 Sentinel-6A'/
data sadline(35) /'67 Sentinel-6B'/
data sadline(36) /'70 METEOSAT-11'/
data sadline(37) /'120 ADEOS'/
data sadline(38) /'121 ADEOS-II'/
data sadline(39) /'122 GCOM-W1'/
data sadline(40) /'140 GOSAT'/
data sadline(41) /'150 GMS-3'/
data sadline(42) /'151 GMS-4'/
data sadline(43) /'152 GMS-5'/
data sadline(44) /'153 GMS'/
data sadline(45) /'154 GMS-2'/
data sadline(46) /'171 MTSAT-1R'/
data sadline(47) /'172 MTSAT-2'/
data sadline(48) /'173 Himawari-8'/
data sadline(49) /'174 Himawari-9'/
data sadline(50) /'200 NOAA-8'/
data sadline(51) /'201 NOAA-9'/
data sadline(52) /'202 NOAA-10'/
data sadline(53) /'203 NOAA-11'/
data sadline(54) /'204 NOAA-12'/
data sadline(55) /'205 NOAA-14'/
data sadline(56) /'206 NOAA-15'/
data sadline(57) /'207 NOAA-16'/
data sadline(58) /'208 NOAA-17'/
data sadline(59) /'209 NOAA-18'/
data sadline(60) /'220 LANDSAT-5'/
data sadline(61) /'221 LANDSAT-4'/
data sadline(62) /'222 LANDSAT-7'/
data sadline(63) /'223 NOAA-19'/
data sadline(64) /'224 NPP'/
data sadline(65) /'225 NOAA-20'/
data sadline(66) /'226 NOAA-21'/
data sadline(67) /'240 DMSP-7'/
data sadline(68) /'241 DMSP-8'/
data sadline(69) /'242 DMSP-9'/
data sadline(70) /'243 DMSP-10'/
data sadline(71) /'244 DMSP-11'/
data sadline(72) /'245 DMSP-12'/
data sadline(73) /'246 DMSP-13'/
data sadline(74) /'247 DMSP-14'/
data sadline(75) /'248 DMSP-15'/
data sadline(76) /'249 DMSP-16'/
data sadline(77) /'250 GOES-6'/
data sadline(78) /'251 GOES-7'/
data sadline(79) /'252 GOES-8'/
data sadline(80) /'253 GOES-9'/
data sadline(81) /'254 GOES-10'/
data sadline(82) /'255 GOES-11'/
data sadline(83) /'256 GOES-12'/
data sadline(84) /'257 GOES-13'/
data sadline(85) /'258 GOES-14'/
data sadline(86) /'259 GOES-15'/
data sadline(87) /'260 JASON-1'/
data sadline(88) /'261 JASON-2'/
data sadline(89) /'262 JASON-3'/
data sadline(90) /'269 Spire-Lemur_3U_Cubesat'/
data sadline(91) /'270 GOES-16'/
data sadline(92) /'271 GOES-17'/
data sadline(93) /'272 GOES-18'/
data sadline(94) /'273 GOES-19'/
data sadline(95) /'281 QUIKSCAT'/
data sadline(96) /'282 TRMM'/
data sadline(97) /'283 CORIOLIS'/
data sadline(98) /'285 DMSP-17'/
data sadline(99) /'286 DMSP-18'/
data sadline(100) /'287 DMSP-19'/
data sadline(101) /'288 GPM-core'/
data sadline(102) /'289 Orbiting-Carbon-Observatory-2(OCO-2,NASA)'/
data sadline(103) /'310 GOMS-1'/
data sadline(104) /'311 GOMS-2'/
data sadline(105) /'320 METEOR-2-21'/
data sadline(106) /'321 METEOR-3-5'/
data sadline(107) /'322 METEOR-3M-1'/
data sadline(108) /'323 METEOR-3M-2'/
data sadline(109) /'341 RESURS-01-4'/
data sadline(110) /'410 KALPANA-1'/
data sadline(111) /'421 Oceansat-2'/
data sadline(112) /'422 ScatSat-1'/
data sadline(113) /'422 ScatSat-1'/
data sadline(114) /'423 Oceansat-3'/
data sadline(115) /'430 INSAT-1B'/
data sadline(116) /'431 INSAT-1C'/
data sadline(117) /'432 INSAT-1D'/
data sadline(118) /'440 Megha-Tropiques'/
data sadline(119) /'441 SARAL'/
data sadline(120) /'450 INSAT-2A'/
data sadline(121) /'451 INSAT-2B'/
data sadline(122) /'452 INSAT-2E'/
data sadline(123) /'470 INSAT-3A'/
data sadline(124) /'471 INSAT-3D'/
data sadline(125) /'472 INSAT-3E'/
data sadline(126) /'473 INSAT-3DR'/
data sadline(127) /'474 INSAT-3DS'/
data sadline(128) /'500 FY-1C'/
data sadline(129) /'501 FY-1D'/
data sadline(130) /'502 Hai-Yang-2A(HY-2A,SOA/NSOAS/China)'/
data sadline(131) /'503 Hai-Yang-2B(HY-2B,SOA/NSOAS/China)'/
data sadline(132) /'510 FY-2'/
data sadline(133) /'512 FY-2B'/
data sadline(134) /'513 FY-2C'/
data sadline(135) /'514 FY-2D'/
data sadline(136) /'515 FY-2E'/
data sadline(137) /'516 FY-2F'/
data sadline(138) /'517 FY-2G'/
data sadline(139) /'518 FY-2H'/
data sadline(140) /'520 FY-3A'/
data sadline(141) /'521 FY-3B'/
data sadline(142) /'522 FY-3C'/
data sadline(143) /'523 FY-3D'/
data sadline(144) /'530 FY-4A'/
data sadline(145) /'700 ITOS-1'/
data sadline(146) /'701 NOAA-1'/
data sadline(147) /'702 NOAA-2'/
data sadline(148) /'703 NOAA-3'/
data sadline(149) /'704 NOAA-4'/
data sadline(150) /'705 NOAA-5'/
data sadline(151) /'706 NOAA-6'/
data sadline(152) /'707 NOAA-7'/
data sadline(153) /'708 TIROS-N'/
data sadline(154) /'710 GOES-(SMS-1)'/
data sadline(155) /'711 GOES-(SMS-2)'/
data sadline(156) /'720 TOPEX'/
data sadline(157) /'721 GFO'/
data sadline(158) /'722 GRACE-A'/
data sadline(159) /'723 GRACE-B'/
data sadline(160) /'724 COSMIC-2-P1'/
data sadline(161) /'725 COSMIC-2-P2'/
data sadline(162) /'726 COSMIC-2-P3'/
data sadline(163) /'727 COSMIC-2-P4'/
data sadline(164) /'728 COSMIC-2-P5'/
data sadline(165) /'729 COSMIC-2-P6'/
data sadline(166) /'731 GOES-1'/
data sadline(167) /'732 GOES-2'/
data sadline(168) /'733 GOES-3'/
data sadline(169) /'734 GOES-4'/
data sadline(170) /'735 GOES-5'/
data sadline(171) /'740 COSMIC-1'/
data sadline(172) /'741 COSMIC-2'/
data sadline(173) /'742 COSMIC-3'/
data sadline(174) /'743 COSMIC-4'/
data sadline(175) /'744 COSMIC-5'/
data sadline(176) /'745 COSMIC-6'/
data sadline(177) /'750 COSMIC-2-E1'/
data sadline(178) /'751 COSMIC-2-E2'/
data sadline(179) /'752 COSMIC-2-E3'/
data sadline(180) /'753 COSMIC-2-E4'/
data sadline(181) /'754 COSMIC-2 E5'/
data sadline(182) /'755 COSMIC-2 E6'/
data sadline(183) /'763 NIMBUS-3'/
data sadline(184) /'764 NIMBUS-4'/
data sadline(185) /'765 NIMBUS-5'/
data sadline(186) /'766 NIMBUS-6'/
data sadline(187) /'767 NIMBUS-7'/
data sadline(188) /'780 ERBS'/
data sadline(189) /'781 UARS'/
data sadline(190) /'782 EARTH-PROBE'/
data sadline(191) /'783 TERRA'/
data sadline(192) /'784 AQUA'/
data sadline(193) /'785 AURA'/
data sadline(194) /'786 C/NOFS'/
data sadline(195) /'787 CALIPSO'/
data sadline(196) /'788 CloudSat'/
data sadline(197) /'800 SUNSAT'/
data sadline(198) /'801 (ISS)'/
data sadline(199) /'802 CFOSAT'/
data sadline(200) /'803 GRACE-C (GRACE-FO)'/
data sadline(201) /'804 GRACE-D (GRACE-FO)'/
data sadline(202) /'810 COMS'/
data sadline(203) /'811 GEO-KOMPSAT-2A'/
data sadline(204) /'812 SCISAT-1'/
data sadline(205) /'813 ODIN'/
data sadline(206) /'820 SAC-C'/
data sadline(207) /'821 SAC-D'/
data sadline(208) /'825 KOMPSAT-5'/
data sadline(209) /'850 TERRA+AQUA'/
data sadline(210) /'851 NOAA16-NOAA19'/
data sadline(211) /'852 METOP1+METOP3'/
data sadline(212) /'853 METEOSAT+DMSP'/
data sadline(213) /'854 Non-specific mixture of geostationary and low earth orbiting satellites'/
data sadline(214) /'855 INSAT3D+INSAT3DR'/
data sidline(1) /'10 BNSC Radiometer AATSR (Advanced along track scanning radiometer)'/
data sidline(2) /'11 BNSC Radiometer ATSR (Along track scanning radiometer)'/
data sidline(3) /'12 BNSC Radiometer ATSR-2 (Along track scanning radiometer - 2)'/
data sidline(4) /'13 BNSC Radiometer MWR (Microwave radiometer)'/
data sidline(5) /'30 CNES Communications ARGOS'/
data sidline(6) /'40 CNES Lidar Laser reflectors'/
data sidline(7) /'41 CNES Lidar DORIS (Doppler orbitography and radio-positioning integrated by satellite)'/
data sidline(8) /'42 CNES Lidar DORIS-NG (Doppler orbitography and radio-positioning integrated by satellite-NG)'/
data sidline(9) /'47 CNES Radar altimeter POSEIDON-1 (SSALT1) (Positioning ocean solid Earth ice dynamics orbiting navigator (single frequency solid state radar altimeter))'/
data sidline(10) /'48 CNES Radar altimeter POSEIDON-2 (SSALT2) (Positioning ocean solid Earth ice dynamics orbiting navigator (double frequency solid state radar altimeter))'/
data sidline(11) /'49 CNES Radar altimeter POSEIDON-3 (SSALT3) (Advanced microwave radiometer)'/
data sidline(12) /'50 CNES Imager radiometer ATSR/M (ATSR/M)'/
data sidline(13) /'51 CNES High resolution optical imager HRG'/
data sidline(14) /'52 CNES Radiometer HRV (High-resolution visible)'/
data sidline(15) /'53 CNES Radiometer HRVIR (High-resolution visible and infrared)'/
data sidline(16) /'54 CNES Radiometer ScaRaB/MV2 (Scanner for Earth radiation budget)'/
data sidline(17) /'55 CNES Radiometer POLDER (POLDER)'/
data sidline(18) /'56 CNES Imaging multi-spectral radiometer IIR (Imaging Infrared Radiometer)'/
data sidline(19) /'57 ESA/EUMETSAT Radar altimeter POSEIDON-4 (High precision altimetry, dual frequency (C and Ku band) pulse-width limited radar altimeter, synthetic-aperture processing, interleaved Low Rate and High Rate)'/
data sidline(20) /'60 CNES Spectrometer VEGETATION (VEGETATION)'/
data sidline(21) /'61 CNES Spectrometer WINDII (WINDII)'/
data sidline(22) /'62 CNES Altimeter AltiKa (Ka band Radar Altimeter)'/
data sidline(23) /'80 CSA Communications RADARSAT DTT'/
data sidline(24) /'81 CSA Communications RADARSAT TTC'/
data sidline(25) /'85 CSA Radar SAR (CSA) (Synthetic aperture radar (CSA))'/
data sidline(26) /'90 CSA Radiometer MOPITT (Measurements of pollution in the troposphere)'/
data sidline(27) /'91 CSA Atmospheric chemistry instrument OSIRIS (Optical spectrograph and Infrared imaging system)'/
data sidline(28) /'92 CSA Limb-scanning sounder ACE-FTS (Atmospheric Chemistry Experiment - Fourier Transform Spectrometer)'/
data sidline(29) /'97 CSIRO Radiometer Panchromatic imager'/
data sidline(30) /'98 CRCSS Atmospheric temperature and humidity sounder GPS receiver'/
data sidline(31) /'102 DLR Radiometer CHAMP GPS sounder (GPS turborogue space receiver (TRSR))'/
data sidline(32) /'103 DLR Radiometer IGOR (Integrated GPS and Occultation Receiver)'/
data sidline(33) /'104 NASA GNSS occultation sounder Tri-G (Triple-G (GPS, Galileo, GLONASS))'/
data sidline(34) /'116 DLR Magnetometer CHAMP gravity package (Accelerometer+GPS) (STAR accelerometer)'/
data sidline(35) /'117 DLR Magnetometer CHAMP magnetometry package (1 scalar+2 vector magnetometer) (Overhauser magnetometer (OVM) and fluxgate magnetometer (FGM))'/
data sidline(36) /'120 ESA Communications ENVISAT Comms (Communications package on ENVISAT)'/
data sidline(37) /'121 ESA Communications ERS Comms (Communication package for ERS)'/
data sidline(38) /'130 ESA Lidar ALADIN (Atmospheric laser doppler instrument)'/
data sidline(39) /'131 ESA Lidar ATLID (Atmospheric lidar)'/
data sidline(40) /'140 ESA Radar AMI/SAR/Image (Active microwave instrumentation image mode)'/
data sidline(41) /'141 ESA Radar AMI/SAR/wave (Active microwave instrumentation wave mode)'/
data sidline(42) /'142 ESA Radar AMI/scatterometer (Active microwave instrumentation wind mode)'/
data sidline(43) /'143 ESA Radar ASAR (ASAR)'/
data sidline(44) /'144 ESA Imaging microwave ASAR (Advanced synthetic aperture radar (image mode))'/
data sidline(45) /'145 ESA Imaging microwave ASAR (Advanced synthetic aperture radar (wave mode))'/
data sidline(46) /'146 ESA Cloud profile and rain radar CPR (Cloud radar)'/
data sidline(47) /'147 ESA Radar RA-2/MWR (Radar altimeter - 2)'/
data sidline(48) /'148 ESA Radar RA/MWR (Radar altimeter)'/
data sidline(49) /'150 ESA Scatterometer SCATTEROMETER (Scatterometer)'/
data sidline(50) /'151 ESA Imaging radar SAR-C (Synthetic Aperture Radar (C-band))'/
data sidline(51) /'152 Cross-nadir scanning SW (Sounder TROPOMI Tropospheric Monitoring Instrument)'/
data sidline(52) /'161 ESA Radiometer MIPAS (Michelson interferometric passive atmosphere sounder)'/
data sidline(53) /'162 ESA Imaging multi-spectral radiometer (passive microwave) MWR-2 (Microwave radiometer-2)'/
data sidline(54) /'163 ESA Atmospheric chemistry instrument SOPRANO (Sub-milimetre observation of processes in the absorption noteworthy for ozone)'/
data sidline(55) /'170 ESA Atmospheric chemistry instrument GOME (Global ozone monitoring experiment)'/
data sidline(56) /'172 ESA Spectrometer GOMOS (Global ozone monitoring by occultation of stars)'/
data sidline(57) /'174 ESA Spectrometer MERIS (Medium resolution imaging spectrometer)'/
data sidline(58) /'175 ESA Spectrometer SCIAMACHY (Scanning imaging absorption spectrometer for atmospheric cartography)'/
data sidline(59) /'176 ESA Radiometer MIRAS (Microwave Imaging Radiometer Using Aperture Synthesis)'/
data sidline(60) /'177 ESA Radar Altimeter SIRAL (SAR/Interferometric Radar Altimeter)'/
data sidline(61) /'178 ESA Radar Altimeter SRAL (Synthetic aperture radar altimeter)'/
data sidline(62) /'179 Moderate resolution optical imager OLCI (Ocean and land colour imager)'/
data sidline(63) /'180 Moderate resolution optical imager SLSTR (Sea and land surface temperature radiometer)'/
data sidline(64) /'181 EUMETSAT Communications METEOSAT Comms (Communications package for METEOSAT)'/
data sidline(65) /'182 EUMETSAT Communications MSG Comms (Communications package for MSG)'/
data sidline(66) /'190 ESA/EUMETSAT Scatterometer ASCAT (Advanced scatterometer)'/
data sidline(67) /'200 EUMETSAT Radiometer GERB (Geostationary Earth radiation budget)'/
data sidline(68) /'202 ESA/EUMETSAT Radiometer GRAS (GNSS receiver for atmospheric sounding)'/
data sidline(69) /'203 EUMETSAT Radiometer MHS (Microwave humidity sounder)'/
data sidline(70) /'205 EUMETSAT Radiometer MVIRI (METEOSAT visible and infrared imager)'/
data sidline(71) /'207 EUMETSAT Radiometer SEVIRI (Spinning enhanced visible and infrared imager)'/
data sidline(72) /'208 EUMETSAT Imaging multi-spectral radiometer (vis/IR) VIRI (VIRI)'/
data sidline(73) /'220 ESA/EUMETSAT Spectrometer GOME-2 (Global ozone monitoring experiment - 2)'/
data sidline(74) /'221 CNES/EUMETSAT Atmospheric temperature and humidity sounder IASI (Infrared atmospheric sounding interferometer)'/
data sidline(75) /'240 CAST Communications DCP (Data-collection platform transponder)'/
data sidline(76) /'245 CAST Radiometer CCD (High-resolution CCD camera)'/
data sidline(77) /'246 INPE Atmospheric temperature and humidity sounder HSB (Humidity sounder/Brazil)'/
data sidline(78) /'248 INPE Imaging multi-spectral radiometer (vis/IR) OBA (Observador Brasileiro da Amazonia)'/
data sidline(79) /'250 CAST Radiometer WFI (Wide field imager)'/
data sidline(80) /'255 CAST Spectrometer IRMSS (Infrared multispectral scanner)'/
data sidline(81) /'260 ISRO Precision orbit BSS & FSS transponders'/
data sidline(82) /'261 ISRO Precision orbit DRT-S&R'/
data sidline(83) /'262 ISRO Communications INSAT Comms (Communications package for INSAT)'/
data sidline(84) /'268 ISRO High resolution optical imager HR-PAN (High-resolution panchromatic camera)'/
data sidline(85) /'269 ISRO Imaging multi-spectral radiometer (passive microwave) MSMR (Multifrequency scanning microwave radiometer)'/
data sidline(86) /'270 ISRO Imaging multi-spectral radiometer (vis/IR) VHRR (Very high resolution radiometer)'/
data sidline(87) /'271 ISRO Imaging multi-spectral radiometer (vis/IR) WiFS (Wide field sensor)'/
data sidline(88) /'275 ISRO High-resolution optical imager AWiFS (Advanced wide field sensor)'/
data sidline(89) /'276 ISRO High-resolution optical imager LISS-I (Linear imaging self scanner - I)'/
data sidline(90) /'277 ISRO High-resolution optical imager LISS-II (Linear imaging self scanner - II)'/
data sidline(91) /'278 ISRO High-resolution optical imager LISS-III (Linear imaging self scanner - III)'/
data sidline(92) /'279 ISRO High-resolution optical imager LISS-IV (Linear imaging self scanner - IV)'/
data sidline(93) /'284 ISRO High-resolution optical imager PAN (Panchromatic sensor)'/
data sidline(94) /'285 ISRO Imaging multi-spectral radiometer (vis/IR) MOS (Modular opto-electronic scanner)'/
data sidline(95) /'286 ISRO Ocean colour instrument OCM (Ocean colour monitor)'/
data sidline(96) /'287 ASI ROSA (Radio Occultation Sounder of the Atmosphere)'/
data sidline(97) /'288 ISRO Scatterometer SCAT (Scatterometer)'/
data sidline(98) /'289 ISRO Optical imager IMG (Imager)'/
data sidline(99) /'290 JMA Communications MTSAT Comms (Communications package for MTSAT)'/
data sidline(100) /'291 JMA Communications Himawari Comms (Communications package for Himawari)'/
data sidline(101) /'294 JMA Imaging multi-spectral radiometer IMAGER/MTSAT-2 (Imager/MTSAT-2)'/
data sidline(102) /'295 JMA Imaging multi-spectral radiometer JAMI (Japanese Advanced Meteorological Imager)'/
data sidline(103) /'296 JMA Imaging multi-spectral radiometer VISSR (Visible and infrared spin scan radiometer)'/
data sidline(104) /'297 JMA Imaging multi-spectral radiometer AHI (Advanced Himawari Imager)'/
data sidline(105) /'300 NASA Lidar GLAS (Geoscience laser altimeter system)'/
data sidline(106) /'301 NASA Precision orbit LRA (Laser retroreflector array)'/
data sidline(107) /'302 NASA Lidar MBLA (Multi beam laser altimeter)'/
data sidline(108) /'303 NASA Lidar CALIOP (Cloud-aerosol lidar with orthogonal polarization)'/
data sidline(109) /'309 NASA Cloud profile and rain radar CPR (Cloudsat) (Cloud profiling radar)'/
data sidline(110) /'312 NASA Radar NSCAT (NASA scatterometer)'/
data sidline(111) /'313 NASA Radar SeaWinds (ADEOS II - NASA scatterometer)'/
data sidline(112) /'314 NASA Radar RapidScat (RapidScat scatterometer)'/
data sidline(113) /'330 NASA Earth radiation budget radiometer ACRIM (Active cavity radiometer irradiance monitor)'/
data sidline(114) /'334 NASA Total and profile ozone BUV (Backscatter ultraviolet instrument)'/
data sidline(115) /'336 NASA High-resolution optical imager ALI (Advanced land imager)'/
data sidline(116) /'347 NASA High-resolution optical imager ASTER (Advanced spaceborne thermal emission and reflection radiometer)'/
data sidline(117) /'348 NASA Earth radiation budget radiometer CERES-2 (Cloud and the Earth radiant energy system)'/
data sidline(118) /'351 NASA Atmospheric temperature and humidity sounder GPSDR (GPS demonstration receiver)'/
data sidline(119) /'353 NASA Total and profile ozone HiRDLS (High-resolution dynamics limb sounder)'/
data sidline(120) /'354 NASA Total and profile ozone HRDI (High-resolution Doppler imager)'/
data sidline(121) /'356 NASA Radiometer LIS (Lightning imaging sensor)'/
data sidline(122) /'358 NASA Magnetic field, Auroal imagery Scintillation boundary PEM (Particle environment monitor)'/
data sidline(123) /'359 NASA Ocean colour instrument SeaWiFS (Sea-viewing wide field-of-view sensor)'/
data sidline(124) /'360 NASA Earth radiation budget radiometer SUSIM (UARS) (Solar ultraviolet irradiance monitor)'/
data sidline(125) /'363 NASA Total and profile ozone SBUV/1 (Solar backscatter ultraviolet 1 instrument)'/
data sidline(126) /'365 NASA Imaging multi-spectral radiometer (passive microwave) TMI (TRMM microwave imager)'/
data sidline(127) /'366 NASA Imaging multi-spectral radiometer (passive microwave) JMR (JASON-1 microwave radiometer)'/
data sidline(128) /'367 NASA Imaging multi-spectral radiometer AMR (Positioning ocean solid Earth ice dynamics orbiting navigator (double frequency solid state radar altimeter))'/
data sidline(129) /'369 NASA Total and profile ozone LIMS (Limb infrared monitor of the stratosphere)'/
data sidline(130) /'370 NASA Total and profile ozone LRIR (Limb radiance inversion radiometer instrument)'/
data sidline(131) /'371 NASA Total and profile ozone EPIC (Earth polychromatic imaging camera)'/
data sidline(132) /'372 NASA Earth radiation budget radiometer NISTAR (NIST advanced radiometer)'/
data sidline(133) /'373 NASA Magnetic field, auroal imagery scintillation boundary Plasma-Mag'/
data sidline(134) /'374 NASA Other XPS (XUV photometer system)'/
data sidline(135) /'375 NASA Imaging multi-spectral radiometer (vis/IR) VIRS (Visible infrared scanner)'/
data sidline(136) /'376 CNES Multiple direction/polarisation radiometer POLDER II (Polarization and directionality of the Earth reflectance - II)'/
data sidline(137) /'377 NASA Earth radiation budget radiometer TIM (Total irradiance monitor)'/
data sidline(138) /'379 NASA Imaging multi-spectral radiometer (vis/IR) WFC (Wide field camera)'/
data sidline(139) /'382 NASA Spectro-radiometer CLAES (Cryogenic limb array etalon spectrometer)'/
data sidline(140) /'383 NASA Spectro-radiometer HALOE (Halogen occultation experiment)'/
data sidline(141) /'384 NASA Spectro-radiometer ISAMS (Improved stratospheric and mesospheric sounder)'/
data sidline(142) /'385 NASA Spectro-radiometer MISR (Multi-angle imaging spectroradiometer)'/
data sidline(143) /'386 NASA Spectro-radiometer MLS (Microwave limb sounder)'/
data sidline(144) /'387 NASA Spectro-radiometer MLS (EOS-Aura) (Microwave limb sounder (EOS-Aura))'/
data sidline(145) /'389 NASA Spectro-radiometer MODIS (Moderate-resolution imaging spectroradiometer)'/
data sidline(146) /'393 NASA Gravity HAIRS (High accuracy inter-satellite ranging system)'/
data sidline(147) /'394 NASA Total and profile ozone OMI (Ozone measuring instrument)'/
data sidline(148) /'395 NASA Radiometer Atmospheric corrector (Atmospheric corrector)'/
data sidline(149) /'396 NASA Radiometer Hyperion (Hyperspectral imager)'/
data sidline(150) /'399 NASA Spectro-radiometer SAGE I (Stratospheric aerosol and gas experiment-I)'/
data sidline(151) /'400 NASA Spectro-radiometer SAGE II (Stratospheric aerosol and gas experiment-II)'/
data sidline(152) /'401 NASA Spectro-radiometer SAGE III (Stratospheric aerosol and gas experiment-III)'/
data sidline(153) /'402 NASA Spectro-radiometer SAMS (Stratospheric and mesospheric sounder)'/
data sidline(154) /'403 NASA Spectro-radiometer SAM-II (Stratospheric aerosol measurement - II)'/
data sidline(155) /'404 NASA Spectro-radiometer IRIS (Infrared interferometer spectrometer)'/
data sidline(156) /'405 NASA Atmospheric temperature and humidity sounder GIFTS (Geosynchronous imaging fourier transform spectrometer)'/
data sidline(157) /'420 NASA Spectrometer AIRS (Atmospheric Infrared sounder)'/
data sidline(158) /'426 NASA Spectrometer SOLSTICE (Solar stellar irradiance comparison experiment)'/
data sidline(159) /'430 NASA Spectrometer TES (Troposhperic emission spectrometer)'/
data sidline(160) /'431 NASA Spectrometer TOMS (Total ozone mapping spectrometer)'/
data sidline(161) /'432 NASA Spectrometer OCO (Orbiting Carbon Observatory)'/
data sidline(162) /'450 JAXA Communications ADEOS Comms (Communications package for ADEOS)'/
data sidline(163) /'451 JAXA Communications DCS (JAXA) (Data collection system (JAXA))'/
data sidline(164) /'453 NASDA Communications GMS Comms (Communications package on GMS)'/
data sidline(165) /'454 NASDA Communications JERS-1 Comms (Communications package for JERS-1)'/
data sidline(166) /'460 NASDA Lidar RIS (Retroreflector in space)'/
data sidline(167) /'461 NASDA Radar PR (Precipitation radar)'/
data sidline(168) /'462 NASDA Imaging microwave radar SAR (Synthetic aperture radar)'/
data sidline(169) /'470 JAXA Imaging microwave radar PALSAR (Phased array type L-band synthetic aperture radar)'/
data sidline(170) /'478 JAXA Imaging multi-spectral radiometer (passive microwave) AMSR2 (Advanced microwave scanning radiometer 2)'/
data sidline(171) /'479 JAXA Imaging multi-spectral radiometer (passive microwave) AMSR-E (Advanced microwave scanning radiometer-EOS)'/
data sidline(172) /'480 JAXA High resolution optical imager PRISM (ALOS) (Panchromatic remote-sensing Instrument for stereo mapping)'/
data sidline(173) /'481 JAXA Radiometer AMSR (Advanced microwave scanning Radiometer)'/
data sidline(174) /'482 NASDA High-resolution optical imager AVNIR (Advanced visible and near infrared radiometer)'/
data sidline(175) /'483 JAXA High-resolution optical imager AVNIR-2 (Advanced visible and near infra-red radiometer type 2)'/
data sidline(176) /'484 JAXA Imager GLI (Global imager)'/
data sidline(177) /'485 NASDA Radiometer MESSR (Multispectral electronic self scanning radiometer)'/
data sidline(178) /'486 NASDA Radiometer MSR (Microwave scanning radiometer)'/
data sidline(179) /'487 NASDA Radiometer OCTS (Ocean color and temperature scanner)'/
data sidline(180) /'488 NASDA Radiometer OPS (Optical sensor)'/
data sidline(181) /'489 NASDA Radiometer VISSR (GMS-5) (Visible and infrared spin scan radiometer (GMS-5))'/
data sidline(182) /'490 NASDA Radiometer VTIR (Visible and thermal infrared radiometer)'/
data sidline(183) /'510 NASDA Spectrometer ILAS-I (Imoroved limb atmosphiric spectrometer)'/
data sidline(184) /'511 NASDA Spectrometer ILAS-II (Improved limb atmosphiric spectrometer)'/
data sidline(185) /'512 NASDA Spectrometer IMG (Inferometric monitor of greenhouse gases)'/
data sidline(186) /'514 NASDA Space environment SEM (Space environment monitor (NASDA))'/
data sidline(187) /'515 JAXA Total and profile ozone SOFIS (Solar occultation Fourier transform spectrometer for Inclined orbit Satellite)'/
data sidline(188) /'516 JAXA Spectrometer TANSO-FTS (Thermal and Near infrared Sensor for carbon Observations ( TANSO) Fourier Transform Spectrometer (FTS))'/
data sidline(189) /'517 JAXA Imager TANSO-CAI (Thermal and Near infrared Sensor for carbon Observations ( TANSO) Cloud and Aerosol Imager (CAI))'/
data sidline(190) /'518 JAXA Cloud and precipitation radar DPR (Dual-frequency precipitation radar)'/
data sidline(191) /'519 JAXA MW imaging/sounding radiometer, conical scanning GMI (GPM microwave imager)'/
data sidline(192) /'530 Spire GNSS occultation sounder SGNOS-A (Spire global navigation satellite system occultation sounder A)'/
data sidline(193) /'531 Spire GNSS occultation sounder SGNOS-B (Spire global navigation satellite system occultation sounder B)'/
data sidline(194) /'532 Spire GNSS occultation sounder SGNOS-C (Spire global navigation satellite system occultation sounder C)'/
data sidline(195) /'533 Spire GNSS occultation sounder SGNOS-D (Spire global navigation satellite system occultation sounder D)'/
data sidline(196) /'540 NOAA Communications DCS (NOAA) (Data-collection system (NOAA))'/
data sidline(197) /'541 NOAA Communications GOES Comms (Communications package on GOES)'/
data sidline(198) /'542 NOAA Communications LANDSAT Comms (Communications package for LANDSAT)'/
data sidline(199) /'543 NOAA Communications NOAA Comms (Communications package for NOAA)'/
data sidline(200) /'544 NOAA Communications S&R (GOES) (Search and rescue)'/
data sidline(201) /'545 NOAA Communications S&R (NOAA) (Search and rescue)'/
data sidline(202) /'546 NOAA Communications WEFAX (Weather facsimile)'/
data sidline(203) /'547 NOAA Spectrometer SEM(GOES) (Space environment monitor)'/
data sidline(204) /'550 NOAA Magnetic field SSM (Special sensor magnetometer)'/
data sidline(205) /'551 NOAA Magnetic field SSJ/4 (Special sensor precipitating plasma monitor)'/
data sidline(206) /'552 NOAA Space environment SSIES-2 (Special sensor ionospheric plasma drift/scintillation meter)'/
data sidline(207) /'553 NOAA Space environment SSB/X-2 (Special sensor gamma ray particle dectector)'/
data sidline(208) /'570 NOAA Radiometer AMSU-A (Advanced microwave sounding unit-A)'/
data sidline(209) /'574 NOAA Radiometer AMSU-B (Advanced microwave sounding unit-B)'/
data sidline(210) /'580 NOAA Radiometer ATOVS (HIRS/3 + AMSU + AVHRR/3) (Advanced TIROS operational vertical sounder)'/
data sidline(211) /'590 NOAA Radiometer AVHRR/2 (Advanced very high-resolution radiometer/2)'/
data sidline(212) /'591 NOAA Radiometer AVHRR/3 (Advanced very high-resolution radiometer/3)'/
data sidline(213) /'592 NOAA Radiometer AVHRR/4 (Advanced very high-resolution radiometer/4)'/
data sidline(214) /'600 NOAA Radiometer ERBE (Earth radiation budget experiment)'/
data sidline(215) /'601 NOAA Radiometer ETM+ (Enhanced thematic mapper)'/
data sidline(216) /'604 NOAA Radiometer HIRS/1 (High-resolution infrared sounder/1)'/
data sidline(217) /'605 NOAA Radiometer HIRS/2 (High-resolution infrared sounder/2)'/
data sidline(218) /'606 NOAA Radiometer HIRS/3 (High-resolution infrared sounder/3)'/
data sidline(219) /'607 NOAA Radiometer HIRS/4 (High-resolution infrared sounder/4)'/
data sidline(220) /'615 NOAA Radiometer IMAGER (Imager)'/
data sidline(221) /'616 NOAA Imaging multi-spectral radiometer (vis/IR) VIIRS (Visible/infrared imager radiometer suite)'/
data sidline(222) /'617 NOAA Imaging multi-spectral radiometer ABI (Advanced baseline imager)'/
data sidline(223) /'617 NOAA Imaging multi-spectral radiometer ABI (Advanced baseline imager)'/
data sidline(224) /'618 NOAA High-resolution optical imager GLM (Geostationary lightning mapper)'/
data sidline(225) /'618 NOAA High-resolution optical imager GLM (Geostationary lightning mapper)'/
data sidline(226) /'620 NOAA Atmospheric temperature and humidity sounder CrIRS/NP (Cross track infrared sounder/NPOESS)'/
data sidline(227) /'621 NOAA Atmospheric temperature and humidity sounder ATMS (Advanced technology microwave sounder)'/
data sidline(228) /'622 NOAA Radiometer MSS (Multispectral scanning system)'/
data sidline(229) /'623 NOAA Radiometer MSU (Microwave sounding unit)'/
data sidline(230) /'624 NOAA Radiometer SBUV/2 (Solar backscattter ultraviolet instrument/2)'/
data sidline(231) /'625 NOAA Radiometer SBUV/3 (Solar backscattter ultraviolet instrument/3)'/
data sidline(232) /'626 NOAA Radiometer SOUNDER (SOUNDER)'/
data sidline(233) /'627 NOAA Radiometer SSU (Stratospheric sounding unit)'/
data sidline(234) /'628 NOAA Radiometer TM (Thematic mapper)'/
data sidline(235) /'629 NOAA Radiometer TOVS (HIRS/2 + MSU + SSU) (TIROS operational vertical sounder)'/
data sidline(236) /'630 NOAA Radiometer VAS (VISSR atmospheric sounder)'/
data sidline(237) /'631 NOAA Radiometer SSZ'/
data sidline(238) /'645 NOAA Spectrometer SEM (Space environment monitor)'/
data sidline(239) /'650 NRSCC Radiometer MVIRSR (10 channel) (Multispectral visible and infrared scan radiometer)'/
data sidline(240) /'651 NRSCC Radiometer MVIRSR (3 channel) (Multispectral visible and infrared scan radiometer)'/
data sidline(241) /'652 NRSCC Radiometer MVIRSR (5 channel) (Multispectral visible and infrared scan radiometer)'/
data sidline(242) /'670 NSAU Radar RLSBO (Side looking microwave radar)'/
data sidline(243) /'680 NSAU High-resolution optical imager MSU-EU (Multi-spectral radiometer with high resolution)'/
data sidline(244) /'681 NSAU Imaging multi-spectral radiometer (vis/IR) MSU-UM (Visible multi-spectral radiometer)'/
data sidline(245) /'682 NSAU Radiometer RM-08 (Imaging microwave radiometer)'/
data sidline(246) /'683 NSAU High-resolution optical imager SU-UMS (Stereo radiometer with high resolution)'/
data sidline(247) /'684 NSAU High-resolution optical imager SU-VR (Visible radiometer with high resolution)'/
data sidline(248) /'685 NSAU Radiometer TRASSER'/
data sidline(249) /'686 SOA Scatterometer SCAT (Scatterometer)'/
data sidline(250) /'687 SOA Radar altimeter ALT (Radar altimeter)'/
data sidline(251) /'688 SOA Microwave radiometer MWI (Microwave radiometer)'/
data sidline(252) /'700 ROSCOSMOS Communications KONDOR-2 (Data collection and transmission system)'/
data sidline(253) /'701 ROSCOSMOS Communications BRK'/
data sidline(254) /'710 ROSCOSMOS Lidar ALISSA (Backscatter lidar)'/
data sidline(255) /'712 ROSCOSMOS Lidar Balkan-2 lidar'/
data sidline(256) /'715 ROSCOSMOS Lidar MK-4'/
data sidline(257) /'716 ROSCOSMOS Lidar MK-4M'/
data sidline(258) /'730 ROSCOSMOS Radar Greben (Radar altimeter)'/
data sidline(259) /'731 ROSCOSMOS Radar SAR-10 (Synthetic aperture radar)'/
data sidline(260) /'732 ROSCOSMOS Radar SAR-3 (Synthetic aperture radar)'/
data sidline(261) /'733 ROSCOSMOS Radar SAR-70 (Synthetic aperture radar)'/
data sidline(262) /'740 ROSCOSMOS Radar SLR-3 (Side looking radar)'/
data sidline(263) /'745 ROSCOSMOS Radar Travers SAR'/
data sidline(264) /'750 ROSCOSMOS Radiometer 174-K (Temperature and humidity profiler)'/
data sidline(265) /'751 ROSCOSMOS Radiometer BTVK (Scanning television radiometer)'/
data sidline(266) /'752 ROSCOSMOS Radiometer Chaika (Scanning infrared radiometer)'/
data sidline(267) /'753 ROSCOSMOS Radiometer DELTA-2 (Multispectral microwave scanner)'/
data sidline(268) /'755 ROSCOSMOS Radiometer IKAR-D (Multispectral microwave scanner)'/
data sidline(269) /'756 ROSCOSMOS Radiometer IKAR-N (Multispectral microwave scanner)'/
data sidline(270) /'757 ROSCOSMOS Radiometer IKAR-P (Multispectral microwave scanner)'/
data sidline(271) /'760 ROSCOSMOS Radiometer ISP'/
data sidline(272) /'761 ROSCOSMOS Radiometer KFA-1000 (Photographic camera)'/
data sidline(273) /'762 ROSCOSMOS Radiometer KFA-200 (Photographic camera)'/
data sidline(274) /'763 ROSCOSMOS Radiometer KFA-3000 (Photographic camera)'/
data sidline(275) /'770 ROSCOSMOS Radiometer Klimat (Scanning infrared radiometer)'/
data sidline(276) /'771 ROSCOSMOS Radiometer Klimat-2 (Scanning infrared radiometer)'/
data sidline(277) /'775 ROSCOSMOS Radiometer MIRAS'/
data sidline(278) /'776 ROSCOSMOS Radiometer MIVZA'/
data sidline(279) /'777 ROSCOSMOS Radiometer MIVZA-M (Microwave scanning radiometer)'/
data sidline(280) /'780 ROSCOSMOS Radiometer MR-2000'/
data sidline(281) /'781 ROSCOSMOS Radiometer MR-2000M'/
data sidline(282) /'785 ROSCOSMOS Radiometer MR-900 (Scanning telephotometer)'/
data sidline(283) /'786 ROSCOSMOS Radiometer MR-900B (Scanning visual band telephotometer)'/
data sidline(284) /'790 ROSCOSMOS Radiometer MSU-E (Multispectral high-resolution electronic scanner)'/
data sidline(285) /'791 ROSCOSMOS Radiometer MSU-E1 (Multispectral high-resolution electronic scanner)'/
data sidline(286) /'792 ROSCOSMOS Radiometer MSU-E2 (Multispectral high-resolution electronic scanner)'/
data sidline(287) /'793 ROSCOSMOS Radiometer MSU-M'/
data sidline(288) /'794 ROSCOSMOS Radiometer MSU-S (Multispectral medium-resolution scanner)'/
data sidline(289) /'795 ROSCOSMOS Radiometer MSU-SK (Multispectral medium-resolution conical scanner)'/
data sidline(290) /'796 ROSCOSMOS Radiometer MSU-V (Multispectral high-resolution conical scanner)'/
data sidline(291) /'810 ROSCOSMOS Radiometer MTZA (Scanning microwave radiometer)'/
data sidline(292) /'815 ROSCOSMOS Imaging multi-spectral radiometer (passive microwave) MZOAS (Scanning microwave radiometer)'/
data sidline(293) /'820 ROSCOSMOS Imaging multi-spectral radiometer (passive microwave) R-225 (Single channel microwave radiometer)'/
data sidline(294) /'821 ROSCOSMOS Radiometer R-400'/
data sidline(295) /'822 ROSCOSMOS Radiometer R-600 (Single channel microwave radiometer)'/
data sidline(296) /'830 ROSCOSMOS Radiometer RMS (Radiation measurement system)'/
data sidline(297) /'835 ROSCOSMOS Radiometer TV camera'/
data sidline(298) /'836 ROSCOSMOS Radiometer SILVA'/
data sidline(299) /'840 ROSCOSMOS Spectro-radiometer SROSMO (Spectroradiometer for ocean monitoring)'/
data sidline(300) /'850 ROSCOSMOS Spectrometer BUFS-2 (Backscatter spectrometer/2)'/
data sidline(301) /'851 ROSCOSMOS Spectrometer BUFS-4 (Backscatter spectrometer/4)'/
data sidline(302) /'855 ROSCOSMOS Spectrometer ISTOK-1 (Infrared spectrometer)'/
data sidline(303) /'856 ROSCOSMOS Spectrometer SFM-2 (Spectrometer to measure direct solar radiation)'/
data sidline(304) /'857 ROSCOSMOS Spectrometer DOPI'/
data sidline(305) /'858 ROSCOSMOS Spectrometer KGI-4'/
data sidline(306) /'859 ROSCOSMOS Spectrometer Ozon-M'/
data sidline(307) /'860 ROSCOSMOS Spectrometer RMK-2'/
data sidline(308) /'900 NOAA Radiometer MAXIE (Magnetospheric atmospheric X-ray imaging experiment)'/
data sidline(309) /'901 NOAA Radiometer OLS (Operational linescan system)'/
data sidline(310) /'905 NOAA Radiometer SSM/I (Mission sensor microwave imager)'/
data sidline(311) /'906 NOAA Radiometer SSM/T-1 (Mission sensor microwave temperature sounder)'/
data sidline(312) /'907 NOAA Radiometer SSM/T-2 (Mission sensor microwave water vapour sounder)'/
data sidline(313) /'908 NOAA Radiometer SSMIS (Special sensor microwave imager sounder)'/
data sidline(314) /'910 NOAA Radiometer SXI (Solar X-ray imager)'/
data sidline(315) /'930 NOAA Spectrometer EHIC (Energetic heavy ion composition experiment)'/
data sidline(316) /'931 NOAA Spectrometer X-ray astronomy payload'/
data sidline(317) /'932 NRSCC Imaging multi-spectral radiometer (vis/IR) IVISSR (FY-2) (Improved multispectral visible and Infrared scan radiometer (5 channels))'/
data sidline(318) /'933 NRSCC Atmospheric temperature and humidity sounder IRAS (Infrared atmospheric sounder)'/
data sidline(319) /'934 NRSCC Atmospheric temperature and humidity sounder MWAS (MicroWave atmospheric sounder)'/
data sidline(320) /'935 NRSCC Atmospheric temperature and humidity sounder IMWAS (Improved MicroWave atmospheric sounder)'/
data sidline(321) /'936 NRSCC Atmospheric temperature and humidity sounder MWHS (Microwave humidity sounder)'/
data sidline(322) /'937 NRSCC Imaging multi-spectral radiometer (vis/IR) MVIRS (Moderate resolution visible and infrared imaging spectroradiometer)'/
data sidline(323) /'938 NRSCC Imaging multi-spectral radiometer (passive microwave) MWRI (Microwave radiation imager)'/
data sidline(324) /'940 ROSCOSMOS Atmospheric temperature and humidity sounder MTVZA-OK (Scanning microwave radiometer)'/
data sidline(325) /'941 CNES Atmospheric temperature and humidity sounder SAPHIR'/
data sidline(326) /'942 CNES Microwave imager MADRAS (Microwave Analysis and Detection of Rain and Atmospheric Structures)'/
data sidline(327) /'943 CNSA Scatterometer SCAT (on CFOSAT) Scatterometer'/
data sidline(328) /'944 NOAA Radar altimeter ALT (Altimeter)'/
data sidline(329) /'945 NOAA Earth radiation budget radiometer TSIS (Total solar irradiance sensor)'/
data sidline(330) /'946 NOAA Imaging multi-spectral radiometer (passive microwave) CMIS (Conical-scanning microwave imager/sounder)'/
data sidline(331) /'947 NOAA Total and profile ozone OMPS (Ozone mapping and profiler suite)'/
data sidline(332) /'948 NOAA Space environment atmospheric temperature and humidity sounder GPSOS (Global positioning system occultation sensor)'/
data sidline(333) /'949 NOAA Magnetic field, Auroal imagery Scintillation boundary SESS (Space environmental sensor suite)'/
data sidline(334) /'950 NRSCC Imaging multi-spectral radiometer (vis/IR) VIRR (Multispectral visible and infrared scan radiometer (10 channels))'/
data sidline(335) /'951 NRSCC Total and profile ozone TOM (Total ozone mapper)'/
data sidline(336) /'952 NRSCC Total and profile ozone OP (Ozone profiler)'/
data sidline(337) /'953 CMA Microwave sounding radiometer, crosstrack scanning MWHS-2 (Microwave humidity sounder-2)'/
data sidline(338) /'954 CMA Microwave sounding radiometer, crosstrack scanning MWTS-2 (Microwave temperature sounder-2)'/
data sidline(339) /'955 CMA Cross-nadir scanning IR sounder HIRAS (Hyperspectral infrared atmospheric sounder))'/
data sidline(340) /'956 CMA Spectrometer SBUS (Solar Backscatter Ultraviolet Sounder)'/
data sidline(341) /'957 CMA Spectrometer TOU (Total Ozone Unit)'/
data sidline(342) /'958 GNSS occultation sounder GNOS (Global navigation satellite system occultation sounder)'/
data sidline(343) /'959 SNSB Limb-scanning sounder SMR (Sub-millimetre radiometer)'/
data sidline(344) /'961 CMA Imaging multi-spectral radiometer AGRI (Advanced Geosynchronous Radiation Imager)'/
data sidline(345) /'962 CMA Atmospheric temperature and humidity sounder GIIRS (Geosynchronous Interferometric Infrared Sounder)'/
data sidline(346) /'963 CMA High-resolution optical imager LMI (Lightning Mapper Imager)'/
data sidline(347) /'964 CMA Space Environment SEP (Space Environment Package)'/
data sidline(348) /'980 KMA Imager AMI (Advanced Meteorological Imager)'/
data sidline(349) /'981 KMA Imager MI (Meteorological Imager)'/
data sidline(350) /'982 KMA Space environment KSEM (Korea Space wEather Monitor)'/
end
