// (C) P.J. Dodd (2018): Distributed under CC BY 4.0 license https://creativecommons.org/licenses/by/4.0/
#include "eventlogic.h"
int eventdefn( person& P,       // person to be acted on
               double& now,     // reference to current time
               event & ev,      // event to be enacted
               eventq& EQ,      // event queue to add consequences to
               NumericVector& Z// ,          // input parameters
               // gsl_rng *r
               ){
  now = ev.t;                   // advance time
  if(P.D["alive"]==1){          // RIP
    updateages(P.A,ev.t-P.tle);    // update all age-like things
    P.tle = ev.t;       // doesn't matter if no change; last event time used for aging
    event todo = {.t=now, .who=P.who, .what=""}; // event to be determined

    // --------- DEFINE LOGIC BELOW HERE (& possibly the "alive" guard): ---------
    // NB users completely responsible for any eligibility/guard logic & consistency between state/input names!!
    if(ev.what=="initialize"){
      // todo.what="die"; todo.t = now + gsl_ran_exponential(r,10);     // time-to-death
      todo.what="die"; todo.t = now - log(Rf_runif(0,1))/Z["mu"];       // time-to-death
      EQ.push(todo);
      // todo.what = "infect"; todo.t = now + gsl_ran_exponential(r,10);// t-to-infection
      todo.what = "infect"; todo.t = now - log(Rf_runif(0,1))/Z["lambda"];// t-to-infection
      EQ.push(todo);
    }
    if(ev.what=="infect"){
      if(P.D["infected"]==0){        // eligible for event
        P.D["infected"] = 1;         // infect!
        P.C["timeofinfection"] = ev.t;
        // todo.what="die"; todo.t = now + gsl_ran_exponential(r,20); // time-to-death if infected
        // todo.what="die"; todo.t = now - log(Rf_runif(0,1))/Z["nu"]; // time-to-death if infected
        // EQ.push(todo);
      }
    }
    if(ev.what=="die"){
      P.D["alive"] = 0;              // recorded as dead
      P.C["timeofdeath"] = ev.t;            // recorded as dead
    }
    if(ev.what=="finalize"){
      // no other changes needed: just for updating ages if alive
    }
    // --------- END LOGIC DEFINITION ---------

  }
  return 0;
}
