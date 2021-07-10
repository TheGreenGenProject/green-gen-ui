module Data.VerificationCode exposing (..)

import Utils.TextUtils as TextUtils
type VerificationCode = VerificationCode Int Int Int

format: VerificationCode -> String
format (VerificationCode s1 s2 s3) =
    (TextUtils.format4Digits s1) ++ "-" ++ (TextUtils.format4Digits s2) ++ "-" ++ (TextUtils.format4Digits s3)

updateSection: VerificationCode -> Int -> Int -> VerificationCode
updateSection (VerificationCode s1 s2 s3) section updated = case section of
    1 -> VerificationCode updated s2 s3
    2 -> VerificationCode s1 updated s3
    3 -> VerificationCode s1 s2 updated
    _ -> VerificationCode s1 s2 s3
