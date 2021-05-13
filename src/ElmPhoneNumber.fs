namespace Elm.Skinney.PhoneNumbers

open Elm
open Elm.Core
open Elm.Core.Basics
open Elm.Core.Maybe
open Elm.Regex

/// What kind of a number are we talking about? Fixed line? Mobile number? Pager number? etc.
type NumberType =
    | FixedLine
    | Mobile
    | TollFree
    | PremiumRate
    | SharedCost
    | PersonalNumber
    | Voip
    | Pager
    | Uan
    | Emergency
    | Voicemail
    | ShortCode
    | StandardRate
    | CarrierSpecific
    | SmsServices
    | NoInternationalDialling

/// Describes a NumberType with an example number, as well as a regex to validate if a given number is of
/// the set numberType
type NumberTypeData =
    { numberType : NumberType
      exampleNumber : string
      pattern : Regex }

/// Contains phone related metadata for a country.
type Country =
    { id : string
      countryCode : string
      internationalPrefix : string Maybe
      nationalPrefix : string Maybe
      generalNumberPattern : Regex
      numberTypes : NumberTypeData list }

/// Pass this record to `valid` or `matches` to define how validation should occur.
/// Validation will occur against the default country as well as the list of other countries,
/// and validation will only occur against the list of number types. Providing a empty list of
/// number types might still succeed, as every country has a general number pattern that will be checked.
/// Any number that doesn't have an international prefix will be treated as if it belongs to the
/// default country.
type ValidationConfig =
    { defaultCountry : Country
      otherCountries : Country list
      types : NumberType Set }

/// A phone number match.
type PhoneNumberMatch = {
    Country: Country
    LocalNumber:string
    NumberTypes:NumberType list }

[<RequireQualifiedAccess>]
module ElmPhoneNumber =
    let private sanitizeNumber str = String.filter (fun c -> c <> ' ') str

    let private regexExactMatch regex str =
        match Regex.find regex str with
        | [ m ] -> m.Match = str
        | _ -> false

    let private localizeNumber country number =
        match country.internationalPrefix with
        | Nothing -> Just number
        | Just prefix ->
            let prefixLength = String.length prefix
            let countryCodeLength = String.length country.countryCode
            if String.startsWith "+" number then
                if number
                   |> String.dropLeft 1
                   |> String.left countryCodeLength
                   |> (=) country.countryCode
                then Just <| String.dropLeft (countryCodeLength + 1) number
                else Nothing
            elif String.startsWith prefix number then
                if number
                   |> String.dropLeft prefixLength
                   |> String.left countryCodeLength
                   |> (=) country.countryCode
                then Just <| String.dropLeft (prefixLength + countryCodeLength) number
                else Nothing
            else Just number

    let private matchingCountry number defaultCountry relevantTypes country =
        let isDefaultCountry = defaultCountry.id = country.id

        match localizeNumber country number with
        | Nothing -> Nothing
        | Just localNumber ->
            if localNumber = number && not isDefaultCountry then Nothing
            else
                let matchingTypes =
                    country.numberTypes
                    |> List.filter (fun nt -> Set.contains nt.numberType relevantTypes)
                    |> List.filter (fun desc -> regexExactMatch desc.pattern localNumber)
                    |> List.map (fun item -> item.numberType)

                match matchingTypes with
                | [] ->
                    if regexExactMatch country.generalNumberPattern localNumber then
                        Just { Country=country; LocalNumber=localNumber; NumberTypes=[] }
                    else Nothing
                | _ -> Just { Country=country; LocalNumber=localNumber; NumberTypes=matchingTypes }

    /// A list of all number types. Use this if you don't care what kind of number something is,
    /// but want to know if the number can belong to a certain country.
    let anyType =
        [ FixedLine; Mobile; TollFree; PremiumRate; SharedCost; PersonalNumber; Voip; Pager; Uan; Emergency;
          Voicemail; ShortCode; StandardRate; CarrierSpecific; SmsServices; NoInternationalDialling ]
        |> Set.ofList

    /// Returns a list of tuples where the first element is a country that the number can belong to,
    /// and the second list contains the different types the number can be. It's possible the the second
    /// element is an empty list, as every country has a general number pattern that will be checked if the
    /// number type cannot be determined.
    let matches config number =
        let sanitizedNumber = sanitizeNumber number
        List.choose (matchingCountry sanitizedNumber config.defaultCountry config.types)
            (config.defaultCountry :: config.otherCountries)

    /// A simple test to see if the provided number matches anything provided by the validation config.
    let valid config number =
        match matches config number with
        | [] -> false
        | _ -> true
