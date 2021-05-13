module Tests.PhoneNumber

open Expecto

open Elm.Skinney.PhoneNumbers
open Tests

[<Tests>]
let tests =
    testList "phone number tests" [
        testCase "GB mobile number should pass" <| fun _ ->
            let config:ValidationConfig = {
                defaultCountry = Countries.countryGB
                otherCountries = []
                types = ElmPhoneNumber.anyType }

            let matches = ElmPhoneNumber.matches config "7400123456"

            "Should not fail" |> Expect.isNonEmpty matches

    ]
