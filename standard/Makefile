FC=gfortran
RANLIB=ranlib
#CPPFLAGS:=$(CPPFLAGS) -g -shared -fPIC

all: libttutil.a

OBJS= \
modules/ttutil.o \
modules/ttutilprefs.o \
\
addinf.o \
addint.o \
addrea.o \
addref.o \
addstf.o \
addstr.o \
ambusy.o \
copfl2.o \
decchk.o \
decdou.o \
decint.o \
decrea.o \
decrec.o \
dectim.o \
delfil.o \
dtardp.o \
dtdpar.o \
dtdpst.o \
dtfsecmp.o \
dtfsedp.o \
dtleap.o \
dtnow.o \
dtsys.o \
entcha.o \
entdch.o \
entddo.o \
entdin.o \
entdou.o \
entdre.o \
entdti.o \
entdyn.o \
enthlp.o \
entint.o \
entrea.o \
enttim.o \
entyno.o \
extens.o \
fatalerr.o \
flexist.o \
flname.o \
fopeng.o \
fopens.o \
getrec.o \
getun.o \
getun2.o \
ifindc.o \
ifindi.o \
istart.o \
iunifl.o \
lint.o \
lowerc.o \
messini.o \
messinq.o \
messwrt.o \
movavr.o \
notnul.o \
openlogf.o \
outar2.o \
outcom.o \
outdat.o \
outplt.o \
outsel.o \
parsword.o \
rchrsrc.o \
rdacha.o \
rdador.o \
rdadou.o \
rdainr.o \
rdaint.o \
rdalog.o \
rdarea.o \
rdarer.o \
rdatim.o \
rddata.o \
rddtmp.o \
rderr.o \
rderri.o \
rdfcha.o \
rdfdor.o \
rdfdou.o \
rdfinr.o \
rdfint.o \
rdflog.o \
rdfrea.o \
rdfrer.o \
rdfrom.o \
rdftim.o \
rdinar.o \
rdindt.o \
rdindx.o \
rdinit.o \
rdinlv.o \
rdinne.o \
rdinqr.o \
rdinqr2.o \
rdinqr3.o \
rdlex.o \
rdmcha.o \
rdmdef.o \
rdmdou.o \
rdmint.o \
rdmlog.o \
rdmrea.o \
rdmtim.o \
rdpars.o \
rdscha.o \
rdsctb.o \
rdsdor.o \
rdsdou.o \
rdsets.o \
rdsinr.o \
rdsint.o \
rdslog.o \
rdsrea.o \
rdsrer.o \
rdstim.o \
rdtmp1.o \
rdtmp2.o \
reaand.o \
reanor.o \
recread.o \
recreadi.o \
recreadt.o \
remove.o \
sfindg.o \
sortch.o \
sortin.o \
str_copy.o \
swpi4.o \
ttuver.o \
unifl.o \
upperc.o \
usedun.o \
ver4_21.o \
warning.o \
words.o \
wracha.o \
wradou.o \
wraint.o \
wralog.o \
wrarea.o \
wratim.o \
wrinit.o \
wrscha.o \
wrsdou.o \
wrsint.o \
wrslog.o \
wrsrea.o \
wrstim.o

libttutil.a: ${OBJS}
	${AR} cru $@ $^
	-ranlib $@

%.o: %.f90
	${FC} ${FFLAGS} ${CPPFLAGS} -c $< -o $@

%.o: %.for
	${FC} ${FFLAGS} ${CPPFLAGS} -c $< -o $@

clean:
	${RM} libttutil.a ${OBJS}

tst: tst.o libttutil.a
	$(FC) -o $@ $^
