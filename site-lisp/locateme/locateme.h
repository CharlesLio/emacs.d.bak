//
//  LocateMe.h
//
//  Created by Robert Harder on 10/22/10.
//  Copyright 2010 Robert Harder. All rights reserved.
//

/* #import <Cocoa/Cocoa.h> */
#import <CoreLocation/CoreLocation.h>
#import <Foundation/Foundation.h>
#import <stdio.h>

#include <emacs-module.h>

typedef struct emacs_runtime emacs_rt;

#define lisp_integer(env, integer)                                             \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    _env_->make_integer(_env_, (integer));                                     \
  })

#define lisp_string(env, string)                                               \
  ({                                                                           \
    emacs_env *_env_ = env;                                                    \
    char *_str_ = string;                                                      \
    _env_->make_string(_env_, _str_, strlen(_str_));                           \
  })

#define lisp_funcall(env, fn_name, ...)                                        \
  ({                                                                           \
    emacs_env *__env = env;                                                    \
    emacs_value __args[] = {__VA_ARGS__};                                      \
    int __nargs = sizeof(__args) / sizeof(emacs_value);                        \
    __env->funcall(__env, __env->intern(__env, (fn_name)), __nargs, __args);   \
  })

#define lisp_provide(env, feature)                                             \
  ({                                                                           \
    emacs_env *__env = env;                                                    \
    emacs_value __feature = __env->intern(__env, feature);                     \
    emacs_value __provide = __env->intern(__env, "provide");                   \
    emacs_value __args[] = {__feature};                                        \
    int __nargs = sizeof(__args) / sizeof(emacs_value);                        \
    __env->funcall(__env, __provide, __nargs, __args);                         \
  })

void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qsym = env->intern(env, name);
  lisp_funcall(env, "fset", Qsym, Sfun);
}

BOOL g_verbose = NO;
NSString *VERSION = @"0.2.1";
NSString *GOOGLE_URL_FORMAT = @"http://maps.google.com/"
                              @"maps?q={LAT},{LON}({HOST},+{TIME})&ie=UTF8&ll={"
                              @"LAT},{LON}&t=roadmap&z=14&iwloc=A&mrt=loc";
NSString *LONG_FORMAT =
    @"Latitude: {LAT}\nLongitude: {LON}\nAltitude (m): {ALT}\nSpeed (m/s): "
    @"{SPD}\nDirection: {DIR}\nHorizontal Accuracy (m): {HAC}\nVertical "
    @"Accuracy (m): {VAC}\nTimestamp: {TIME}\nHostname: {HOST}";

@interface LocateMe : NSObject <CLLocationManagerDelegate> {

  CLLocationManager *locationManager;
  NSMutableArray *locationMeasurements;

  CLLocation *bestEffortAtLocation;
  NSString *userFormat;
  BOOL goodLocationFound;
  BOOL outputAsGoogleURL;
}

@property(nonatomic) BOOL goodLocationFound;
@property(nonatomic) BOOL outputAsGoogleURL;
@property(nonatomic, retain) NSString *userFormat;
@property(nonatomic, retain) CLLocation *bestEffortAtLocation;

- (id)init;
- (void)dealloc;
- (void)stopUpdatingLocation;
- (void)outputLocation:(CLLocation *)loc asGoogleURL:(BOOL)asGoogle;

@end
