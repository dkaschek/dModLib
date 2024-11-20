/** Code auto-generated by cOde 1.1.1 **/
#include <R.h> 
 #include <math.h> 

static double parms[78];
static double forc[0];
static double cons[0];
static double range[2];

#define nGridpoints 2 
#define nSplines 0 
#define precision 1e-05 

#define p1 parms[0] 
 #define pEpoR parms[1] 
 #define lambda1 parms[2] 
 #define lambda2 parms[3] 
 #define p5 parms[4] 
 #define p2 parms[5] 
 #define p3 parms[6] 
 #define p4 parms[7] 
 #define y0_0 parms[8] 
 #define y1_0 parms[9] 
 #define y2_0 parms[10] 
 #define y3_0 parms[11] 
 #define y4_0 parms[12] 
 #define y5_0 parms[13] 
 #define y6_0 parms[14] 
 #define y7_0 parms[15] 
 #define y8_0 parms[16] 
 #define y9_0 parms[17] 
 #define y10_0 parms[18] 
 #define y11_0 parms[19] 
 #define y12_0 parms[20] 
 #define y13_0 parms[21] 
 #define y14_0 parms[22] 
 #define y15_0 parms[23] 
 #define y16_0 parms[24] 
 #define y17_0 parms[25] 
 #define y18_0 parms[26] 
 #define y19_0 parms[27] 
 #define y20_0 parms[28] 
 #define y21_0 parms[29] 
 #define y22_0 parms[30] 
 #define y23_0 parms[31] 
 #define y24_0 parms[32] 
 #define y25_0 parms[33] 
 #define y26_0 parms[34] 
 #define y27_0 parms[35] 
 #define y28_0 parms[36] 
 #define y29_0 parms[37] 
 #define y30_0 parms[38] 
 #define y31_0 parms[39] 
 #define y32_0 parms[40] 
 #define y33_0 parms[41] 
 #define y34_0 parms[42] 
 #define y35_0 parms[43] 
 #define y36_0 parms[44] 
 #define y37_0 parms[45] 
 #define y38_0 parms[46] 
 #define y39_0 parms[47] 
 #define y40_0 parms[48] 
 #define y41_0 parms[49] 
 #define y42_0 parms[50] 
 #define y43_0 parms[51] 
 #define y44_0 parms[52] 
 #define y45_0 parms[53] 
 #define y46_0 parms[54] 
 #define y47_0 parms[55] 
 #define y48_0 parms[56] 
 #define y49_0 parms[57] 
 #define y50_0 parms[58] 
 #define y51_0 parms[59] 
 #define y52_0 parms[60] 
 #define y53_0 parms[61] 
 #define y54_0 parms[62] 
 #define y55_0 parms[63] 
 #define y56_0 parms[64] 
 #define y57_0 parms[65] 
 #define y58_0 parms[66] 
 #define y59_0 parms[67] 
 #define y60_0 parms[68] 
 #define y61_0 parms[69] 
 #define y62_0 parms[70] 
 #define y63_0 parms[71] 
 #define y64_0 parms[72] 
 #define y65_0 parms[73] 
 #define y66_0 parms[74] 
 #define y67_0 parms[75] 
 #define y68_0 parms[76] 
 #define y69_0 parms[77] 
#define tmin range[0]
#define tmax range[1]


void jakstat_s_initmod(void (* odeparms)(int *, double *)) {
	 int N=78;
	 odeparms(&N, parms);
}

void jakstat_s_initforc(void (* odeforcs)(int *, double *)) {
	 int N=0;
	 odeforcs(&N, forc);
}

/** Derivatives (ODE system) **/
void jakstat_s_derivs (int *n, double *t, double *y, double *ydot, double *RPAR, int *IPAR) {

	 double time = *t;

	 ydot[0] = -1.0*(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0])+1.0*(p5*y[4]);
 	 ydot[1] = 1.0*(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0])-2.0*(p2*pow(y[1],2.0));
 	 ydot[2] = 1.0*(p2*pow(y[1],2.0))-1.0*(p3*y[2]);
 	 ydot[3] = 1.0*(p3*y[2])-1.0*(p4*y[3]);
 	 ydot[4] = 2.0*(p4*y[3])-1.0*(p5*y[4]);
 	 ydot[5] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[5])+(p5)*(y[9]);
 	 ydot[6] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[5])+(-(2.0*(p2*(2.0*y[1]))))*(y[6]);
 	 ydot[7] = (p2*(2.0*y[1]))*(y[6])+(-p3)*(y[7]);
 	 ydot[8] = (p3)*(y[7])+(-p4)*(y[8]);
 	 ydot[9] = (2.0*p4)*(y[8])+(-p5)*(y[9]);
 	 ydot[10] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[10])+(p5)*(y[14]);
 	 ydot[11] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[10])+(-(2.0*(p2*(2.0*y[1]))))*(y[11]);
 	 ydot[12] = (p2*(2.0*y[1]))*(y[11])+(-p3)*(y[12]);
 	 ydot[13] = (p3)*(y[12])+(-p4)*(y[13]);
 	 ydot[14] = (2.0*p4)*(y[13])+(-p5)*(y[14]);
 	 ydot[15] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[15])+(p5)*(y[19]);
 	 ydot[16] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[15])+(-(2.0*(p2*(2.0*y[1]))))*(y[16]);
 	 ydot[17] = (p2*(2.0*y[1]))*(y[16])+(-p3)*(y[17]);
 	 ydot[18] = (p3)*(y[17])+(-p4)*(y[18]);
 	 ydot[19] = (2.0*p4)*(y[18])+(-p5)*(y[19]);
 	 ydot[20] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[20])+(p5)*(y[24]);
 	 ydot[21] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[20])+(-(2.0*(p2*(2.0*y[1]))))*(y[21]);
 	 ydot[22] = (p2*(2.0*y[1]))*(y[21])+(-p3)*(y[22]);
 	 ydot[23] = (p3)*(y[22])+(-p4)*(y[23]);
 	 ydot[24] = (2.0*p4)*(y[23])+(-p5)*(y[24]);
 	 ydot[25] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[25])+(p5)*(y[29]);
 	 ydot[26] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[25])+(-(2.0*(p2*(2.0*y[1]))))*(y[26]);
 	 ydot[27] = (p2*(2.0*y[1]))*(y[26])+(-p3)*(y[27]);
 	 ydot[28] = (p3)*(y[27])+(-p4)*(y[28]);
 	 ydot[29] = (2.0*p4)*(y[28])+(-p5)*(y[29]);
 	 ydot[30] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[30])+(p5)*(y[34])-(pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0]);
 	 ydot[31] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[30])+(-(2.0*(p2*(2.0*y[1]))))*(y[31])+pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0];
 	 ydot[32] = (p2*(2.0*y[1]))*(y[31])+(-p3)*(y[32]);
 	 ydot[33] = (p3)*(y[32])+(-p4)*(y[33]);
 	 ydot[34] = (2.0*p4)*(y[33])+(-p5)*(y[34]);
 	 ydot[35] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[35])+(p5)*(y[39])-(p1*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0]);
 	 ydot[36] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[35])+(-(2.0*(p2*(2.0*y[1]))))*(y[36])+p1*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)*y[0];
 	 ydot[37] = (p2*(2.0*y[1]))*(y[36])+(-p3)*(y[37]);
 	 ydot[38] = (p3)*(y[37])+(-p4)*(y[38]);
 	 ydot[39] = (2.0*p4)*(y[38])+(-p5)*(y[39]);
 	 ydot[40] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[40])+(p5)*(y[44])-(p1*pEpoR*(3.0*(exp(-time*lambda1)*time*exp(-time*lambda2)*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),2.0)))*y[0]);
 	 ydot[41] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[40])+(-(2.0*(p2*(2.0*y[1]))))*(y[41])+p1*pEpoR*(3.0*(exp(-time*lambda1)*time*exp(-time*lambda2)*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),2.0)))*y[0];
 	 ydot[42] = (p2*(2.0*y[1]))*(y[41])+(-p3)*(y[42]);
 	 ydot[43] = (p3)*(y[42])+(-p4)*(y[43]);
 	 ydot[44] = (2.0*p4)*(y[43])+(-p5)*(y[44]);
 	 ydot[45] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[45])+(p5)*(y[49])+p1*pEpoR*(3.0*((1.0-exp(-time*lambda1))*(exp(-time*lambda2)*time)*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),2.0)))*y[0];
 	 ydot[46] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[45])+(-(2.0*(p2*(2.0*y[1]))))*(y[46])-(p1*pEpoR*(3.0*((1.0-exp(-time*lambda1))*(exp(-time*lambda2)*time)*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),2.0)))*y[0]);
 	 ydot[47] = (p2*(2.0*y[1]))*(y[46])+(-p3)*(y[47]);
 	 ydot[48] = (p3)*(y[47])+(-p4)*(y[48]);
 	 ydot[49] = (2.0*p4)*(y[48])+(-p5)*(y[49]);
 	 ydot[50] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[50])+(p5)*(y[54])+y[4];
 	 ydot[51] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[50])+(-(2.0*(p2*(2.0*y[1]))))*(y[51]);
 	 ydot[52] = (p2*(2.0*y[1]))*(y[51])+(-p3)*(y[52]);
 	 ydot[53] = (p3)*(y[52])+(-p4)*(y[53]);
 	 ydot[54] = (2.0*p4)*(y[53])+(-p5)*(y[54])-y[4];
 	 ydot[55] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[55])+(p5)*(y[59]);
 	 ydot[56] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[55])+(-(2.0*(p2*(2.0*y[1]))))*(y[56])-(2.0*pow(y[1],2.0));
 	 ydot[57] = (p2*(2.0*y[1]))*(y[56])+(-p3)*(y[57])+pow(y[1],2.0);
 	 ydot[58] = (p3)*(y[57])+(-p4)*(y[58]);
 	 ydot[59] = (2.0*p4)*(y[58])+(-p5)*(y[59]);
 	 ydot[60] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[60])+(p5)*(y[64]);
 	 ydot[61] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[60])+(-(2.0*(p2*(2.0*y[1]))))*(y[61]);
 	 ydot[62] = (p2*(2.0*y[1]))*(y[61])+(-p3)*(y[62])-y[2];
 	 ydot[63] = (p3)*(y[62])+(-p4)*(y[63])+y[2];
 	 ydot[64] = (2.0*p4)*(y[63])+(-p5)*(y[64]);
 	 ydot[65] = (-(p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0)))*(y[65])+(p5)*(y[69]);
 	 ydot[66] = (p1*pEpoR*pow(((1.0-exp(-time*lambda1))*exp(-time*lambda2)),3.0))*(y[65])+(-(2.0*(p2*(2.0*y[1]))))*(y[66]);
 	 ydot[67] = (p2*(2.0*y[1]))*(y[66])+(-p3)*(y[67]);
 	 ydot[68] = (p3)*(y[67])+(-p4)*(y[68])-y[3];
 	 ydot[69] = (2.0*p4)*(y[68])+(-p5)*(y[69])+2.0*y[3];

}
