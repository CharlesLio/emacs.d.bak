//
//  LocateMe.m
//
//  Created by Robert Harder on 10/22/10.
//  Copyright 2010 Robert Harder. All rights reserved.
//

#import "locateme.h"

int plugin_is_gpl_compatible;

static const char *module_name = "locateme";

emacs_value get_location(emacs_env *env, long nargs, emacs_value args[],
                         void *data);

int emacs_module_init(emacs_rt *ert) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  char *msg = NULL;
  asprintf(&msg, "loading dynamic module [%s]...", module_name);
  /* g_string_printf(msg, "loading dynamic module [%s]...", module_name); */
  emacs_env *env = ert->get_environment(ert);
  lisp_funcall(env, "message", lisp_string(env, msg));
  /* g_string_free(msg, true); */
  free(msg);
  emacs_value get_location_fun =
      env->make_function(env, 0, 0, get_location, "get user shell path", NULL);
  bind_function(env, "get-location", get_location_fun);
  lisp_provide(env, module_name);

  [pool drain];
  return 0;
}

emacs_value get_location(emacs_env *env, long nargs, emacs_value args[],
                         void *data) {
  return env->intern(env, "nil");
}

/**
 * Process command line arguments and execute program.
 */
int processArguments(int argc, const char *argv[]) {

  NSString *userFormat = nil;

  int i;

  LocateMe *loc = [[[LocateMe alloc] init] retain];

  if (userFormat != nil) {
    loc.userFormat = userFormat;
  }

  do {
  } while (!loc.goodLocationFound &&
           [[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode
                                    beforeDate:[NSDate distantFuture]]);
  [loc release];

  return 0;
}

@implementation LocateMe

@synthesize goodLocationFound;
@synthesize outputAsGoogleURL;
@synthesize bestEffortAtLocation;
@synthesize userFormat;

- (id)init {
  self = [super init];
  if (self) {
    goodLocationFound = FALSE;
    outputAsGoogleURL = FALSE;
    userFormat = nil;
    locationMeasurements = [[[NSMutableArray alloc] init] retain];
    locationManager = [[[CLLocationManager alloc] init] retain];
    locationManager.delegate = self;
    locationManager.desiredAccuracy = kCLLocationAccuracyKilometer;
    locationManager.distanceFilter = 10;
    [locationManager startUpdatingLocation];
  }
  return self;
}

- (void)dealloc {
  [locationManager release];
  [locationMeasurements release];

  [super dealloc];
}

- (void)stopUpdatingLocation {
  [locationManager stopUpdatingLocation];
  // printf( "%s\n", [[self.bestEffortAtLocation description] UTF8String] );
  self.goodLocationFound = YES;
  [self outputLocation:self.bestEffortAtLocation
           asGoogleURL:self.outputAsGoogleURL];
  CFRunLoopStop(CFRunLoopGetCurrent());
}

- (NSString *)urlencode:(NSString *)raw {

  // Available as of 10.9
  NSString *encodedString =
      [raw stringByAddingPercentEncodingWithAllowedCharacters:
               [NSCharacterSet URLQueryAllowedCharacterSet]];

  // Deprecated as of 10.11
  //    NSString * encodedString = (NSString
  //    *)CFURLCreateStringByAddingPercentEscapes(
  //                                                                                   NULL,
  //                                                                                   (CFStringRef)raw,
  //                                                                                   NULL,
  //                                                                                   (CFStringRef)@"!*'();:@&=+$,/?%#[]",
  //                                                                                   kCFStringEncodingUTF8 );
  return encodedString;
}

- (void)outputLocation:(CLLocation *)loc asGoogleURL:(BOOL)asGoogle {

  if (self.userFormat == nil) {
    printf("%s\n", [[loc description] UTF8String]);
  } else {
    NSMutableString *format = [userFormat mutableCopy];

    [format
        replaceOccurrencesOfString:@"{LAT}"
                        withString:[NSString
                                       stringWithFormat:@"%f",
                                                        loc.coordinate.latitude]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format
        replaceOccurrencesOfString:@"{LON}"
                        withString:[NSString
                                       stringWithFormat:@"%f", loc.coordinate
                                                                   .longitude]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format replaceOccurrencesOfString:@"{ALT}"
                            withString:[NSString
                                           stringWithFormat:@"%f", loc.altitude]
                               options:0
                                 range:NSMakeRange(0, [format length])];
    [format
        replaceOccurrencesOfString:@"{SPD}"
                        withString:[NSString stringWithFormat:@"%f", loc.speed]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format
        replaceOccurrencesOfString:@"{DIR}"
                        withString:[NSString stringWithFormat:@"%f", loc.course]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format
        replaceOccurrencesOfString:@"{HAC}"
                        withString:[NSString
                                       stringWithFormat:@"%f",
                                                        loc.horizontalAccuracy]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format
        replaceOccurrencesOfString:@"{VAC}"
                        withString:[NSString
                                       stringWithFormat:@"%f",
                                                        loc.verticalAccuracy]
                           options:0
                             range:NSMakeRange(0, [format length])];
    [format replaceOccurrencesOfString:@"{TIME}"
                            withString:[loc.timestamp description]
                               options:0
                                 range:NSMakeRange(0, [format length])];
    [format replaceOccurrencesOfString:@"{HOST}"
                            withString:[[NSHost currentHost] name]
                               options:0
                                 range:NSMakeRange(0, [format length])];

    printf("%s\n", [format UTF8String]);
  }
  /*
  if( asGoogle ){
      NSString *gurl = [NSString
  stringWithFormat:@"http://maps.google.com/maps?q=%f,%f(%@,+%@)&ie=UTF8&ll=%f,%f&t=roadmap&z=14&iwloc=A&mrt=loc",
                        loc.coordinate.latitude,
                        loc.coordinate.longitude,
                        [self urlencode:[[NSHost currentHost] name]],
                        [self urlencode:[loc.timestamp description]],
                        loc.coordinate.latitude,
                        loc.coordinate.longitude
                        ];
      printf( "%s\n", [gurl UTF8String] );

  } else {
      printf( "%s\n", [[loc description] UTF8String] );
  }
  */
}

#pragma mark -
#pragma mark Location Manager Interactions

- (void)locationManager:(CLLocationManager *)manager
    didUpdateToLocation:(CLLocation *)newLocation
           fromLocation:(CLLocation *)oldLocation {

  // store all of the measurements, just so we can see what kind of data we
  // might receive
  [locationMeasurements addObject:newLocation];

  // test the age of the location measurement to determine if the measurement is
  // cached in most cases you will not want to rely on cached measurements
  // NSTimeInterval locationAge = -[newLocation.timestamp timeIntervalSinceNow];
  // if (locationAge > 15.0) {
  //    NSLog(@"Old age %f", locationAge);
  //    return;
  //}

  // test that the horizontal accuracy does not indicate an invalid measurement
  if (newLocation.horizontalAccuracy < 0) {
    return;
  }

  // test the measurement to see if it is more accurate than the previous
  // measurement
  if (self.bestEffortAtLocation == nil ||
      self.bestEffortAtLocation.horizontalAccuracy >
          newLocation.horizontalAccuracy) {

    // store the location as the "best effort"
    self.bestEffortAtLocation = newLocation;

    // test the measurement to see if it meets the desired accuracy
    //
    // IMPORTANT!!! kCLLocationAccuracyBest should not be used for comparison
    // with location coordinate or altitidue accuracy because it is a negative
    // value. Instead, compare against some predetermined "real" measure of
    // acceptable accuracy, or depend on the timeout to stop updating. This
    // sample depends on the timeout.
    //
    if (newLocation.horizontalAccuracy <= locationManager.desiredAccuracy) {
      // we have a measurement that meets our requirements, so we can stop
      // updating the location
      //
      [self stopUpdatingLocation];
    }
  }
}

- (void)locationManager:(CLLocationManager *)manager
       didFailWithError:(NSError *)error {
  // The location "unknown" error simply means the manager is currently unable
  // to get the location. We can ignore this error for the scenario of getting a
  // single location fix, because we already have a timeout that will stop the
  // location manager to save power.
  if ([error code] != kCLErrorLocationUnknown) {
    NSLog(@"[error code] != kCLErrorLocationUnknown: %@", error);
  }
  NSLog(@"Error: %@", error);
}

@end
