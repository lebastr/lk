#include <math.h>
#include "lafler_ffi.h"

inline static double min (double x, double y) {
  return (x < y ? x : y);
}

inline static double max (double x, double y) {
  return (x > y ? x : y);
}

static double foldBins (bin *bs, int n_bs){
  int j, p;
  double v0, v1, acc, d, sum, v_2, var;
  j = 0;
  while (j < n_bs && bs[j].empty_flag == 0){
    j++;
  }

  if (j >= n_bs) { return -1.0; }

  v0 = bs[j].value;
  //  printf ("v0: %f\n", v0);
  sum = v0;
  v_2 = v0*v0;
  acc = 0.0;
  p = 1;
  j++;

  for (; j < n_bs; j++){
    if (bs[j].empty_flag == 0)
      continue;

    p++;
    v1 = bs[j].value;
    //    printf ("v1: %f\n", v1);
    d = v1 - v0;
    acc += d*d;
    sum += v1;
    v_2 += v1*v1;
    v0 = v1;
  }
  
  var = (v_2 - sum*sum / (double) p) / (double) (p-1);
  //  printf ("p: %d, v_2: %f, sum: %f, acc: %f\n", p, v_2, sum, acc);
  return ((double) p * var / acc);
}

static void scatterByBins (double freq, observation *obs, int n_obs, bin *bs, int n_bs) {
  int j, i;
  double phase;
  for (j=0; j < n_obs; j++){
    phase = freq*obs[j].time;
    phase = phase - (double) floor(phase);
    i = 1 + floor(phase * (double) n_obs);
    if (bs[i].empty_flag == 0){
      bs[i].empty_flag = 1;
      bs[i].value = obs[j].value;
    } else {
      if (i % 2 == 0) {
		bs[i].value = max (obs[j].value,bs[i].value);
      } else {
		bs[i].value = min (obs[j].value,bs[i].value);
      }
    }
  }
}

double VarianceIndex (double freq, observation *obs, int n_obs, bin *bs, int n_bs) {
  int j;
  for (j = 0; j < n_bs; j++){
    bs[j].empty_flag = 0;
  }

  scatterByBins (freq, obs, n_obs, bs, n_bs);
  return (foldBins (bs, n_bs));
}

void RunLaflerKinman (double *freq, int n_freq, observation *obs, int n_obs, bin *bs, int n_bs, variance_index *idx) {
  int j;
  double f;
  for (j = 0; j < n_freq; j++) {
    f = freq[j];
    idx[j].freq = f;
    idx[j].v_idx = VarianceIndex (f, obs, n_obs, bs, n_bs);
  }
}
