{ fetchbower, buildEnv }:
buildEnv { name = "bower-env"; ignoreCollisions = true; paths = [
  (fetchbower "foundation" "5.5.3" "*" "0ilwi48vn69rc117piqnkqb16l97nkn72jghiiznf6hqvwnbscx2")
  (fetchbower "fontawesome" "4.5.0" "*" "0x6a2miym4i2m2hlgrari4m4i56w9qnsxlw89v9cvaj535d578f0")
]; }
