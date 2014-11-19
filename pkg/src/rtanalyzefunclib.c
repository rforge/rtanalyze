//Rtanalyze C-source.
//Wouter D. Weeda.
//Libary of functions for ARF


#include<R.h>
#include<Rmath.h>
#include<R_ext/Utils.h>


void spotdiffusion(double *a, double *t0, double *P, double *sda, double *rd, double *st0, double *sz, double *eta, double *cond, int *ntrials, int *maxstep, double *RTerr, double *RTcorr)
{
	//code adapted from python-code generously provided by Dr. C.N. White.

	double z,step,ta,st,tar,s,taas,th,xxx,dell,ib,kka,nn,kk,xt,P_trial,qq,temp,n_one,nm;
	double spotsd[*maxstep],inner[*maxstep],outer[*maxstep],cent[*maxstep],drift_t;
	int i,n,j,stepje;

	GetRNGstate();

	//initial checks
	if(*a<0.0) *a = 0.0;

	//set equidistant starting point and drift coefficient, center of target.
	z = *a/2;
	s = .1;
	tar = 2.5;

	//set scalings
	step = .001;
	ta = .001;
	st = ta/step; //time in ms for each step in random walk

	//make spotlight
	for(i=1;i<(*maxstep+1);i++)	{
		spotsd[i] = (*sda)-(*rd)*i;
		if(spotsd[i]<.001) spotsd[i]=.001;

		inner[i] = 2*(pnorm(2.0,tar,spotsd[i],1,0)-pnorm(1.0,tar,spotsd[i],1,0));
		cent[i] = pnorm(3.0,tar,spotsd[i],1,0)-pnorm(2.0,tar,spotsd[i],1,0);
		outer[i] = 1.0 - inner[i] - cent[i];

	}

	//scalings from variables to probability in stepupdown
	taas = sqrt(ta)/s;
	th = *a/2.0;
	xxx=z-th;
	dell = sqrt(s*s*(*a)); //dell is the scaling of starting point and boundary separation into number of steps
	ib = round(th/dell);
	kka = round(ib+xxx/dell);
	nn= 2*ib;


	//Simulate
	for(i=1;i<(*ntrials+1);i++)	{

		n=0;
		kk = kka+round((unif_rand()-.5)*(*sz)/dell); //start at real starting point

		if(kk<2.0) kk=2.0;
		if(kk>(nn-2.0)) kk = nn - 2.0;

		xt = round(((*t0)*st/ta) + ((*st0)*(unif_rand()*.5)*st/ta)); //transform non-decision time to ms

		if((*eta)!=0) {
			P_trial = rnorm(*P,*eta);	//pick value of drift from norm dist with mean P and SD eta
		} else {
			P_trial = *P;
		}

		for(j=1;j<(*maxstep+1);j++)	{

			drift_t = (inner[j]*(*cond)*P_trial)+(cent[j]*P_trial)+(outer[j]*(*cond)*P_trial); //drift_t value of drift for each time step

			qq = (1.0-drift_t*taas)/2.0;
			temp = unif_rand();


			if(temp<qq) {
				kk = kk-1.0;
				stepje = 0;
			} else {
				if(temp>=qq) {
					kk = kk + 1.0;
					stepje = 1;
				}
			}

			//Rprintf("> %f %f (%d)\n",temp,qq,stepje);

			n++;

			if(kk<=0.0 | kk>=nn) {

				nm = (double) n;
				//low bound hit (error)
				if(kk<=0.0) {
					n_one = round(nm*st+xt);// # scale number of steps into ms and add nondecision time
					RTerr[i] = n_one;

					//Rprintf("> %f %f ERR\n",kk,nn);
					//Rprintf("ERROR\n");
				}
				if(kk>=nn) {
					n_one = round(nm*st+xt);
					RTcorr[i] = n_one;
					//Rprintf("> %f %f CORR\n",kk,nn);
				}

				break;
			}

		}
	}

	PutRNGstate();
	//end
}


