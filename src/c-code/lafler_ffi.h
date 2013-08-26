#ifndef __LAFLER_H__
#define __LAFLER_H__

typedef struct {
  double time;
  double value; 
} observation;

typedef struct {
  int empty_flag;
  double value;
} bin;

typedef struct {
  double freq;
  double v_idx;
} variance_index;

double VarianceIndex (double freq, observation *obs, int n_obs, bin *bs, int n_bs);

void RunLaflerKinman (double *freq, int n_freq, observation *obs, int n_obs, bin *bs, int n_bs, variance_index *idx);

#endif /* __LAFLER_H__ */
