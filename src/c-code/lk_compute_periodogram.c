#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_N_OBS 100000

#include <string.h>
#define N_FORK 2
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
              
/* Auxiliary definitions */
#define MAX(a, b) (((a)>(b)) ? (a) : (b))
#define MIN(a, b) (((a)<(b)) ? (a) : (b))
  
  
int get_number_of_cpu_cores(){
 char string[1024]; // string in the /proc/cpuinfo file
 int n_cores=N_FORK; // default number of cores
 int i; // a counter 
 FILE *proc_cpuinfo_file;
 proc_cpuinfo_file=fopen("/proc/cpuinfo","r");
 if( NULL==proc_cpuinfo_file ){
  fprintf(stderr,"WARNING: cannot open /proc/cpuinfo !\nThe number of CPU cores is set to the default value of %d.\nThis default value is defined in src/limits.h\n",n_cores);
  return n_cores;
 }
 n_cores=0;
 // for each string in /proc/cpuinfo ...
 while(NULL!=fgets(string,1024,proc_cpuinfo_file)){
  // cut the first work out of the string
  for(i=0;i<strlen(string);i++)
   if( string[i]==':' ){
   string[i]='\0';
   break;
  }      
  // test if this the right key
  if( 0==strcmp(string,"processor\t") )n_cores++;
 }
 fclose(proc_cpuinfo_file);
 if( n_cores==0 ){
  n_cores=N_FORK;
  fprintf(stderr,"WARNING: cannot parse /proc/cpuinfo !\nThe number of CPU cores is set to the default value of %d.\nThis default value is defined in src/limits.h\n",n_cores);
  return n_cores;
 }
 fprintf(stderr,"The number of processor cores determined from /proc/cpuinfo is %d.\n",n_cores);
 return n_cores;
}

struct Obs{
 float phase;
 unsigned int n;
};

static int compare_phases(const void *obs11, const void *obs22){
 struct Obs *obs1=(struct Obs *)obs11;
 struct Obs *obs2=(struct Obs *)obs22;
 if( obs1->phase<obs2->phase )return -1;
 return 1;
}

void get_min_max(double *x, int N, double *min, double *max){
 int i;
 (*min)=(*max)=x[0];
 for(i=1;i<N;i++){
  if( x[i]<(*min) )(*min)=x[i];
  if( x[i]>(*max) )(*max)=x[i];
 }
 return;
}

double compute_theta(double *jd, double *m, unsigned int N_obs, double f, double M){
 unsigned int i; 
 struct Obs *obs=malloc(N_obs*sizeof(struct Obs));
 double sum1,sum2;
 double jdi_over_period;

 for(sum2=0.0,i=0;i<N_obs;i++){
  jdi_over_period=(jd[i]-jd[0])*f;
  obs[i].phase=(float)( jdi_over_period-(double)(int)(jdi_over_period) );
  if( obs[i].phase<0.0 )obs[i].phase+=1.0;
  obs[i].n=i; // index
  sum2+=(m[i]-M)*(m[i]-M);
 }
 
 qsort(obs, N_obs, sizeof(struct Obs), compare_phases);
 
 for(sum1=0.0,i=1;i<N_obs;i++){
  sum1+=(m[obs[i].n]-m[obs[i-1].n])*(m[obs[i].n]-m[obs[i-1].n]);
 }

 free(obs);
 
 return sum2/sum1; //1.0/theta;     
}

int main(int argc, char **argv){
 
 if( argc<4 ){
  fprintf(stderr,"Usage:  %s lightcurve.dat Pmax Pmin Step\n",argv[0]);
  return 1;
 }

 FILE *lcfile;
 FILE *periodogramfile;
 FILE *finalperiodogramfile;
 
 double pmax=atof(argv[2]);
 double pmin=atof(argv[3]);
 double step=atof(argv[4]);

 double fmin=1.0/pmax;
 double fmax=1.0/pmin;
 double df;

 // the number of frequencies will be determined later based on the JD range of observations
 unsigned int N_freq;
 double *freq;
 double *theta;
 
 unsigned int N_obs;
 double *jd=malloc(MAX_N_OBS*sizeof(double));
 double *m=malloc(MAX_N_OBS*sizeof(double));
 
 double jdmin,jdmax,T;

 unsigned int i;

 double M;
 
 int n_fork=get_number_of_cpu_cores(); // number of parallel threads
 int i_fork=0; // counter for fork
 pid_t pid;
 int pid_status;
 int *child_pids;
 child_pids=malloc(n_fork*sizeof(int));

 int imin,imax,in; 
 char string[512];                                        
 
 lcfile=fopen(argv[1],"r");
 N_obs=0;
 while(-1<fscanf(lcfile,"%lf %lf",&jd[N_obs],&m[N_obs]))N_obs++;
 fclose(lcfile);

 get_min_max( jd, N_obs, &jdmin, &jdmax);
 T=jdmax-jdmin;
 fprintf(stderr,"JDstart%lf JDstop%lf Time interval (days)= %lf\nComputing, wait a minute please...\n",jdmin,jdmax,T);

 // compute mean magnitude (M) here
 for(M=0.0,i=0;i<N_obs;i++){
  M+=m[i];
 }
 M=M/(double)N_obs;
 
 // Get number of frequencies in the spectrum
 df=step/T;
 N_freq=(int)((fmax-fmin)/df+0.5);

 in=N_freq/n_fork;
 for(i_fork=0;i_fork<n_fork;){
//  fprintf(stderr,"AAAA i_fork=%d\n",i_fork);
  imin=in*i_fork;
  imax=imin+in;//MIN(N_freq,imin+in);
  if( N_freq-imax<in )imax=N_freq;
  i_fork++;
  pid=fork();
  if( pid==0 ){
   // if child
   //fprintf(stderr,"imin=%d imax=%d in=%d N_freq=%d i_fork=%d n_fork=%d pid=%d\n",imin,imax,in,N_freq,i_fork,n_fork,pid);
   
   //!!!
   //exit(0);
   
   freq=malloc((imax-imin)*sizeof(double));
   theta=malloc((imax-imin)*sizeof(double));
 
   // Main loop
   for(i=imin;i<imax;i++){
    if( i-imin!=0 )
     freq[i-imin]=freq[i-imin-1]+df;
    else
     freq[0]=fmin+imin*df; 
    theta[i-imin]=compute_theta(jd,m,N_obs,freq[i-imin],M);
   }
 
   // write results
   sprintf(string,"lk_%d.periodogram",getpid());
   periodogramfile=fopen(string,"w");
   for(i=imin;i<imax;i++){
    fprintf(periodogramfile,"%lf %lf\n",freq[i-imin],theta[i-imin]);
   }
   fclose(periodogramfile);

   free(freq);
   free(theta);
   exit(0);
  }
  else{
   // if parent
   child_pids[i_fork-1]=pid;
   if( i_fork==n_fork ){
    //sprintf(string,"lk_%d.periodogram",getpid());
    sprintf(string,"lk.periodogram");
    finalperiodogramfile=fopen(string,"w");
    for(i_fork=0;i_fork<n_fork;i_fork++){
     pid=child_pids[i_fork];
     waitpid(pid,&pid_status,0);
     sprintf(string,"lk_%d.periodogram",pid);
     //fprintf(stderr,"lk_%d.periodogram\n",pid);
     periodogramfile=fopen(string,"r");
     while(-1<fscanf(periodogramfile,"%lf %lf",&jd[0],&m[0]))
      fprintf(finalperiodogramfile,"%lf %lf\n",jd[0],m[0]);
     fclose(periodogramfile);
     unlink(string);
    }
    fclose(finalperiodogramfile);
    fprintf(stderr,"Parallel processig complete!\n");
   }
  }
 }
   
 free(jd);
 free(m);

 free(child_pids);

 return 0;
}
