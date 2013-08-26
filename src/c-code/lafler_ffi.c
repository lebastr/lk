#include <math.h>
#include "lafler_ffi.h"

inline static double min (double x, double y) {
  return (x < y ? x : y);
}

inline static double max (double x, double y) {
  return (x > y ? x : y);
}

static int skipBins (bin *bs, int n_bs, int counter) {
  int j = counter;
  while (j < n_bs && bs[j].empty_flag == 0){
    j++;
  }
  return j;
}

static double foldBins (bin *bs, int n_bs){
  int j0, j1, p;
  double v0, v1, acc, d, sum, v_2, var;
  j0 = skipBins (bs, n_bs, 0);
  if (j0 >= n_bs) { return 0.0; }
  acc = 0.0;
  v0 = bs[j0].value;
  sum = v0;
  v_2 = v0*v0;
  p = 1;
  while (1) {
    j1 = skipBins (bs, n_bs, j0+1);
    if (j1 >= n_bs) { 
      var = v_2 - sum*sum / (double) p;
      return (var / acc);
    }
    p++;
    v1 = bs[j1].value;
    d = v1 - v0;
    acc += d*d;
    sum += v1;
    v_2 += v1*v1;
    v0 = v1;
    j0 = j1;
  }
  return acc;
}

static void scatterByBins (double freq, observation *obs, int n_obs, bin *bs, int n_bs) {
  int j, i;
  double phase;
  for (j=0; j < n_obs; j++){
    phase = freq*obs[j].time;
    phase = phase - (double) floor(phase);
    i = 1 + (int) (phase * (double) n_bs) % n_bs;
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
