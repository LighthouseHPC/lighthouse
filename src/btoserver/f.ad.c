/************************** DISCLAIMER ********************************/
/*                                                                    */
/*   This file was generated on 02/02/06 15:37:22 by the version of   */
/*   ADIC compiled on  06/10/05 18:10:38                              */
/*                                                                    */
/*   ADIC was prepared as an account of work sponsored by an          */
/*   agency of the United States Government and the University of     */
/*   Chicago.  NEITHER THE AUTHOR(S), THE UNITED STATES GOVERNMENT    */
/*   NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, INCLUDING */
/*   ANY OF THEIR EMPLOYEES OR OFFICERS, MAKES ANY WARRANTY, EXPRESS  */
/*   OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR */
/*   THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY INFORMATION OR  */
/*   PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE */
/*   PRIVATELY OWNED RIGHTS.                                          */
/*                                                                    */
/**********************************************************************/
#include "ad_deriv.h"
void   ad_f(DERIV_TYPE  *ad_var_ret,DERIV_TYPE  x) {
DERIV_TYPE  y;
    double  ad_loc_0;
    double  ad_loc_1;
    double  ad_loc_2;
    double  ad_adj_0;
    {
        ad_loc_0 = 2 * DERIV_val(x);
        ad_loc_1 = ad_loc_0 * DERIV_val(x);
        ad_loc_2 = ad_loc_1 - 3.12;
        ad_adj_0 = 2 * DERIV_val(x);
        ad_grad_axpy_2(&(y), ad_adj_0, &(x), ad_loc_0, &(x));
        DERIV_val(y) = ad_loc_2;
    }
    {
        ad_grad_axpy_copy(&(*ad_var_ret), &(y));
        DERIV_val(*ad_var_ret) = DERIV_val(y);
    }
    return;
}
void   ad_AD_Init(int  arg0) {
    ad_AD_GradInit(arg0);

}
void   ad_AD_Final() {
    ad_AD_GradFinal();

}
