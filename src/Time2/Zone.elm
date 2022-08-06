module Time2.Zone exposing
    ( version
    , get
    , names
    , africa__abidjan, africa__accra, africa__addis_ababa, africa__algiers, africa__asmara, africa__bamako, africa__bangui, africa__banjul, africa__bissau, africa__blantyre, africa__brazzaville, africa__bujumbura, africa__cairo, africa__casablanca, africa__ceuta, africa__conakry, africa__dakar, africa__dar_es_salaam, africa__djibouti, africa__douala, africa__el_aaiun, africa__freetown, africa__gaborone, africa__harare, africa__johannesburg, africa__juba, africa__kampala, africa__khartoum, africa__kigali, africa__kinshasa, africa__lagos, africa__libreville, africa__lome, africa__luanda, africa__lubumbashi, africa__lusaka, africa__malabo, africa__maputo, africa__maseru, africa__mbabane, africa__mogadishu, africa__monrovia, africa__nairobi, africa__ndjamena, africa__niamey, africa__nouakchott, africa__ouagadougou, africa__porto_novo, africa__sao_tome, africa__tripoli, africa__tunis, africa__windhoek, america__adak, america__anchorage, america__anguilla, america__antigua, america__araguaina, america__argentina__buenos_aires, america__argentina__catamarca, america__argentina__cordoba, america__argentina__jujuy, america__argentina__la_rioja, america__argentina__mendoza, america__argentina__rio_gallegos, america__argentina__salta, america__argentina__san_juan, america__argentina__san_luis, america__argentina__tucuman, america__argentina__ushuaia, america__aruba, america__asuncion, america__atikokan, america__bahia, america__bahia_banderas, america__barbados, america__belem, america__belize, america__blanc_sablon, america__boa_vista, america__bogota, america__boise, america__cambridge_bay, america__campo_grande, america__cancun, america__caracas, america__cayenne, america__cayman, america__chicago, america__chihuahua, america__costa_rica, america__creston, america__cuiaba, america__curacao, america__danmarkshavn, america__dawson, america__dawson_creek, america__denver, america__detroit, america__dominica, america__edmonton, america__eirunepe, america__el_salvador, america__fort_nelson, america__fortaleza, america__glace_bay, america__goose_bay, america__grand_turk, america__grenada, america__guadeloupe, america__guatemala, america__guayaquil, america__guyana, america__halifax, america__havana, america__hermosillo, america__indiana__indianapolis, america__indiana__knox, america__indiana__marengo, america__indiana__petersburg, america__indiana__tell_city, america__indiana__vevay, america__indiana__vincennes, america__indiana__winamac, america__inuvik, america__iqaluit, america__jamaica, america__juneau, america__kentucky__louisville, america__kentucky__monticello, america__kralendijk, america__la_paz, america__lima, america__los_angeles, america__lower_princes, america__maceio, america__managua, america__manaus, america__marigot, america__martinique, america__matamoros, america__mazatlan, america__menominee, america__merida, america__metlakatla, america__mexico_city, america__miquelon, america__moncton, america__monterrey, america__montevideo, america__montserrat, america__nassau, america__new_york, america__nipigon, america__nome, america__noronha, america__north_dakota__beulah, america__north_dakota__center, america__north_dakota__new_salem, america__nuuk, america__ojinaga, america__panama, america__pangnirtung, america__paramaribo, america__phoenix, america__port_au_prince, america__port_of_spain, america__porto_velho, america__puerto_rico, america__punta_arenas, america__rainy_river, america__rankin_inlet, america__recife, america__regina, america__resolute, america__rio_branco, america__santarem, america__santiago, america__santo_domingo, america__sao_paulo, america__scoresbysund, america__sitka, america__st_barthelemy, america__st_johns, america__st_kitts, america__st_lucia, america__st_thomas, america__st_vincent, america__swift_current, america__tegucigalpa, america__thule, america__thunder_bay, america__tijuana, america__toronto, america__tortola, america__vancouver, america__whitehorse, america__winnipeg, america__yakutat, america__yellowknife, antarctica__casey, antarctica__davis, antarctica__dumontdurville, antarctica__macquarie, antarctica__mawson, antarctica__mcmurdo, antarctica__palmer, antarctica__rothera, antarctica__syowa, antarctica__troll, antarctica__vostok, arctic__longyearbyen, asia__aden, asia__almaty, asia__amman, asia__anadyr, asia__aqtau, asia__aqtobe, asia__ashgabat, asia__atyrau, asia__baghdad, asia__bahrain, asia__baku, asia__bangkok, asia__barnaul, asia__beirut, asia__bishkek, asia__brunei, asia__chita, asia__choibalsan, asia__colombo, asia__damascus, asia__dhaka, asia__dili, asia__dubai, asia__dushanbe, asia__famagusta, asia__gaza, asia__hebron, asia__ho_chi_minh, asia__hong_kong, asia__hovd, asia__irkutsk, asia__istanbul, asia__jakarta, asia__jayapura, asia__jerusalem, asia__kabul, asia__kamchatka, asia__karachi, asia__kathmandu, asia__khandyga, asia__kolkata, asia__krasnoyarsk, asia__kuala_lumpur, asia__kuching, asia__kuwait, asia__macau, asia__magadan, asia__makassar, asia__manila, asia__muscat, asia__nicosia, asia__novokuznetsk, asia__novosibirsk, asia__omsk, asia__oral, asia__phnom_penh, asia__pontianak, asia__pyongyang, asia__qatar, asia__qostanay, asia__qyzylorda, asia__riyadh, asia__sakhalin, asia__samarkand, asia__seoul, asia__shanghai, asia__singapore, asia__srednekolymsk, asia__taipei, asia__tashkent, asia__tbilisi, asia__tehran, asia__thimphu, asia__tokyo, asia__tomsk, asia__ulaanbaatar, asia__urumqi, asia__ust_nera, asia__vientiane, asia__vladivostok, asia__yakutsk, asia__yangon, asia__yekaterinburg, asia__yerevan, atlantic__azores, atlantic__bermuda, atlantic__canary, atlantic__cape_verde, atlantic__faroe, atlantic__madeira, atlantic__reykjavik, atlantic__south_georgia, atlantic__st_helena, atlantic__stanley, australia__adelaide, australia__brisbane, australia__broken_hill, australia__darwin, australia__eucla, australia__hobart, australia__lindeman, australia__lord_howe, australia__melbourne, australia__perth, australia__sydney, europe__amsterdam, europe__andorra, europe__astrakhan, europe__athens, europe__belgrade, europe__berlin, europe__bratislava, europe__brussels, europe__bucharest, europe__budapest, europe__busingen, europe__chisinau, europe__copenhagen, europe__dublin, europe__gibraltar, europe__guernsey, europe__helsinki, europe__isle_of_man, europe__istanbul, europe__jersey, europe__kaliningrad, europe__kiev, europe__kirov, europe__lisbon, europe__ljubljana, europe__london, europe__luxembourg, europe__madrid, europe__malta, europe__mariehamn, europe__minsk, europe__monaco, europe__moscow, europe__nicosia, europe__oslo, europe__paris, europe__podgorica, europe__prague, europe__riga, europe__rome, europe__samara, europe__san_marino, europe__sarajevo, europe__saratov, europe__simferopol, europe__skopje, europe__sofia, europe__stockholm, europe__tallinn, europe__tirane, europe__ulyanovsk, europe__uzhgorod, europe__vaduz, europe__vatican, europe__vienna, europe__vilnius, europe__volgograd, europe__warsaw, europe__zagreb, europe__zaporozhye, europe__zurich, indian__antananarivo, indian__chagos, indian__christmas, indian__cocos, indian__comoro, indian__kerguelen, indian__mahe, indian__maldives, indian__mauritius, indian__mayotte, indian__reunion, pacific__apia, pacific__auckland, pacific__bougainville, pacific__chatham, pacific__chuuk, pacific__easter, pacific__efate, pacific__fakaofo, pacific__fiji, pacific__funafuti, pacific__galapagos, pacific__gambier, pacific__guadalcanal, pacific__guam, pacific__honolulu, pacific__kanton, pacific__kiritimati, pacific__kosrae, pacific__kwajalein, pacific__majuro, pacific__marquesas, pacific__midway, pacific__nauru, pacific__niue, pacific__norfolk, pacific__noumea, pacific__pago_pago, pacific__palau, pacific__pitcairn, pacific__pohnpei, pacific__port_moresby, pacific__rarotonga, pacific__saipan, pacific__tahiti, pacific__tarawa, pacific__tongatapu, pacific__wake, pacific__wallis
    )

{-| This library provides time zone data from the `2022a` release of the IANA
Time Zone Database.

@docs version


# Usage


## Find a zone

@docs get


## Zone names

@docs names


## Zones

@docs africa__abidjan, africa__accra, africa__addis_ababa, africa__algiers, africa__asmara, africa__bamako, africa__bangui, africa__banjul, africa__bissau, africa__blantyre, africa__brazzaville, africa__bujumbura, africa__cairo, africa__casablanca, africa__ceuta, africa__conakry, africa__dakar, africa__dar_es_salaam, africa__djibouti, africa__douala, africa__el_aaiun, africa__freetown, africa__gaborone, africa__harare, africa__johannesburg, africa__juba, africa__kampala, africa__khartoum, africa__kigali, africa__kinshasa, africa__lagos, africa__libreville, africa__lome, africa__luanda, africa__lubumbashi, africa__lusaka, africa__malabo, africa__maputo, africa__maseru, africa__mbabane, africa__mogadishu, africa__monrovia, africa__nairobi, africa__ndjamena, africa__niamey, africa__nouakchott, africa__ouagadougou, africa__porto_novo, africa__sao_tome, africa__tripoli, africa__tunis, africa__windhoek, america__adak, america__anchorage, america__anguilla, america__antigua, america__araguaina, america__argentina__buenos_aires, america__argentina__catamarca, america__argentina__cordoba, america__argentina__jujuy, america__argentina__la_rioja, america__argentina__mendoza, america__argentina__rio_gallegos, america__argentina__salta, america__argentina__san_juan, america__argentina__san_luis, america__argentina__tucuman, america__argentina__ushuaia, america__aruba, america__asuncion, america__atikokan, america__bahia, america__bahia_banderas, america__barbados, america__belem, america__belize, america__blanc_sablon, america__boa_vista, america__bogota, america__boise, america__cambridge_bay, america__campo_grande, america__cancun, america__caracas, america__cayenne, america__cayman, america__chicago, america__chihuahua, america__costa_rica, america__creston, america__cuiaba, america__curacao, america__danmarkshavn, america__dawson, america__dawson_creek, america__denver, america__detroit, america__dominica, america__edmonton, america__eirunepe, america__el_salvador, america__fort_nelson, america__fortaleza, america__glace_bay, america__goose_bay, america__grand_turk, america__grenada, america__guadeloupe, america__guatemala, america__guayaquil, america__guyana, america__halifax, america__havana, america__hermosillo, america__indiana__indianapolis, america__indiana__knox, america__indiana__marengo, america__indiana__petersburg, america__indiana__tell_city, america__indiana__vevay, america__indiana__vincennes, america__indiana__winamac, america__inuvik, america__iqaluit, america__jamaica, america__juneau, america__kentucky__louisville, america__kentucky__monticello, america__kralendijk, america__la_paz, america__lima, america__los_angeles, america__lower_princes, america__maceio, america__managua, america__manaus, america__marigot, america__martinique, america__matamoros, america__mazatlan, america__menominee, america__merida, america__metlakatla, america__mexico_city, america__miquelon, america__moncton, america__monterrey, america__montevideo, america__montserrat, america__nassau, america__new_york, america__nipigon, america__nome, america__noronha, america__north_dakota__beulah, america__north_dakota__center, america__north_dakota__new_salem, america__nuuk, america__ojinaga, america__panama, america__pangnirtung, america__paramaribo, america__phoenix, america__port_au_prince, america__port_of_spain, america__porto_velho, america__puerto_rico, america__punta_arenas, america__rainy_river, america__rankin_inlet, america__recife, america__regina, america__resolute, america__rio_branco, america__santarem, america__santiago, america__santo_domingo, america__sao_paulo, america__scoresbysund, america__sitka, america__st_barthelemy, america__st_johns, america__st_kitts, america__st_lucia, america__st_thomas, america__st_vincent, america__swift_current, america__tegucigalpa, america__thule, america__thunder_bay, america__tijuana, america__toronto, america__tortola, america__vancouver, america__whitehorse, america__winnipeg, america__yakutat, america__yellowknife, antarctica__casey, antarctica__davis, antarctica__dumontdurville, antarctica__macquarie, antarctica__mawson, antarctica__mcmurdo, antarctica__palmer, antarctica__rothera, antarctica__syowa, antarctica__troll, antarctica__vostok, arctic__longyearbyen, asia__aden, asia__almaty, asia__amman, asia__anadyr, asia__aqtau, asia__aqtobe, asia__ashgabat, asia__atyrau, asia__baghdad, asia__bahrain, asia__baku, asia__bangkok, asia__barnaul, asia__beirut, asia__bishkek, asia__brunei, asia__chita, asia__choibalsan, asia__colombo, asia__damascus, asia__dhaka, asia__dili, asia__dubai, asia__dushanbe, asia__famagusta, asia__gaza, asia__hebron, asia__ho_chi_minh, asia__hong_kong, asia__hovd, asia__irkutsk, asia__istanbul, asia__jakarta, asia__jayapura, asia__jerusalem, asia__kabul, asia__kamchatka, asia__karachi, asia__kathmandu, asia__khandyga, asia__kolkata, asia__krasnoyarsk, asia__kuala_lumpur, asia__kuching, asia__kuwait, asia__macau, asia__magadan, asia__makassar, asia__manila, asia__muscat, asia__nicosia, asia__novokuznetsk, asia__novosibirsk, asia__omsk, asia__oral, asia__phnom_penh, asia__pontianak, asia__pyongyang, asia__qatar, asia__qostanay, asia__qyzylorda, asia__riyadh, asia__sakhalin, asia__samarkand, asia__seoul, asia__shanghai, asia__singapore, asia__srednekolymsk, asia__taipei, asia__tashkent, asia__tbilisi, asia__tehran, asia__thimphu, asia__tokyo, asia__tomsk, asia__ulaanbaatar, asia__urumqi, asia__ust_nera, asia__vientiane, asia__vladivostok, asia__yakutsk, asia__yangon, asia__yekaterinburg, asia__yerevan, atlantic__azores, atlantic__bermuda, atlantic__canary, atlantic__cape_verde, atlantic__faroe, atlantic__madeira, atlantic__reykjavik, atlantic__south_georgia, atlantic__st_helena, atlantic__stanley, australia__adelaide, australia__brisbane, australia__broken_hill, australia__darwin, australia__eucla, australia__hobart, australia__lindeman, australia__lord_howe, australia__melbourne, australia__perth, australia__sydney, europe__amsterdam, europe__andorra, europe__astrakhan, europe__athens, europe__belgrade, europe__berlin, europe__bratislava, europe__brussels, europe__bucharest, europe__budapest, europe__busingen, europe__chisinau, europe__copenhagen, europe__dublin, europe__gibraltar, europe__guernsey, europe__helsinki, europe__isle_of_man, europe__istanbul, europe__jersey, europe__kaliningrad, europe__kiev, europe__kirov, europe__lisbon, europe__ljubljana, europe__london, europe__luxembourg, europe__madrid, europe__malta, europe__mariehamn, europe__minsk, europe__monaco, europe__moscow, europe__nicosia, europe__oslo, europe__paris, europe__podgorica, europe__prague, europe__riga, europe__rome, europe__samara, europe__san_marino, europe__sarajevo, europe__saratov, europe__simferopol, europe__skopje, europe__sofia, europe__stockholm, europe__tallinn, europe__tirane, europe__ulyanovsk, europe__uzhgorod, europe__vaduz, europe__vatican, europe__vienna, europe__vilnius, europe__volgograd, europe__warsaw, europe__zagreb, europe__zaporozhye, europe__zurich, indian__antananarivo, indian__chagos, indian__christmas, indian__cocos, indian__comoro, indian__kerguelen, indian__mahe, indian__maldives, indian__mauritius, indian__mayotte, indian__reunion, pacific__apia, pacific__auckland, pacific__bougainville, pacific__chatham, pacific__chuuk, pacific__easter, pacific__efate, pacific__fakaofo, pacific__fiji, pacific__funafuti, pacific__galapagos, pacific__gambier, pacific__guadalcanal, pacific__guam, pacific__honolulu, pacific__kanton, pacific__kiritimati, pacific__kosrae, pacific__kwajalein, pacific__majuro, pacific__marquesas, pacific__midway, pacific__nauru, pacific__niue, pacific__norfolk, pacific__noumea, pacific__pago_pago, pacific__palau, pacific__pitcairn, pacific__pohnpei, pacific__port_moresby, pacific__rarotonga, pacific__saipan, pacific__tahiti, pacific__tarawa, pacific__tongatapu, pacific__wake, pacific__wallis

-}

import Backward
import Dict exposing (Dict)
import Etcetera
import Maybe exposing (Maybe)
import Time exposing (Month(..), Weekday(..))
import Time2
import Time2.Zone.Specification exposing (Clock(..), DateTime, DayOfMonth(..), Rule, Zone, ZoneRules(..), ZoneState)


{-| What release of the IANA Time Zone Database is this data from?
-}
version : String
version =
    "2022a"


minYear : Int
minYear =
    1970


maxYear : Int
maxYear =
    2037


fromSpecification : String -> Zone -> Time2.Zone
fromSpecification name zone =
    let
        ( descending, bottom ) =
            zone |> Time2.Zone.Specification.toOffsets minYear maxYear
    in
    Time2.customZone name descending bottom


{-| Get [`Time2.Zone`][zone] of the given name, with deprecated names changed to new names.

[zone]: /packages/pd9333/elm-time2/latest/Time2#Zone

    import Dict
    import Etcetera
    import Time2


    get "Africa/Asmera"
    --> Just (africa__nairobi ())

    get "America/New_York"
    --> Just (america__new_york ())

    get "UTC"
    --> Dict.get "Etc/UTC" Etcetera.zones
    -->     |> Maybe.map (\f -> f())

-}
get : String -> Maybe Time2.Zone
get name =
    let
        name_ =
            Dict.get name Backward.deprecatedNames
                |> Maybe.withDefault name
    in
    Dict.get name_ zones
        |> (\a ->
                case a of
                    Just _ ->
                        a

                    Nothing ->
                        Dict.get name_ Etcetera.zones
           )
        |> Maybe.map (\f -> f ())


{-| All zone names.
-}
names : List String
names =
    Dict.keys zones


zones : Dict String (() -> Time2.Zone)
zones =
    [ ( "Africa/Abidjan", africa__abidjan )
    , ( "Africa/Accra", africa__accra )
    , ( "Africa/Addis_Ababa", africa__addis_ababa )
    , ( "Africa/Algiers", africa__algiers )
    , ( "Africa/Asmara", africa__asmara )
    , ( "Africa/Bamako", africa__bamako )
    , ( "Africa/Bangui", africa__bangui )
    , ( "Africa/Banjul", africa__banjul )
    , ( "Africa/Bissau", africa__bissau )
    , ( "Africa/Blantyre", africa__blantyre )
    , ( "Africa/Brazzaville", africa__brazzaville )
    , ( "Africa/Bujumbura", africa__bujumbura )
    , ( "Africa/Cairo", africa__cairo )
    , ( "Africa/Casablanca", africa__casablanca )
    , ( "Africa/Ceuta", africa__ceuta )
    , ( "Africa/Conakry", africa__conakry )
    , ( "Africa/Dakar", africa__dakar )
    , ( "Africa/Dar_es_Salaam", africa__dar_es_salaam )
    , ( "Africa/Djibouti", africa__djibouti )
    , ( "Africa/Douala", africa__douala )
    , ( "Africa/El_Aaiun", africa__el_aaiun )
    , ( "Africa/Freetown", africa__freetown )
    , ( "Africa/Gaborone", africa__gaborone )
    , ( "Africa/Harare", africa__harare )
    , ( "Africa/Johannesburg", africa__johannesburg )
    , ( "Africa/Juba", africa__juba )
    , ( "Africa/Kampala", africa__kampala )
    , ( "Africa/Khartoum", africa__khartoum )
    , ( "Africa/Kigali", africa__kigali )
    , ( "Africa/Kinshasa", africa__kinshasa )
    , ( "Africa/Lagos", africa__lagos )
    , ( "Africa/Libreville", africa__libreville )
    , ( "Africa/Lome", africa__lome )
    , ( "Africa/Luanda", africa__luanda )
    , ( "Africa/Lubumbashi", africa__lubumbashi )
    , ( "Africa/Lusaka", africa__lusaka )
    , ( "Africa/Malabo", africa__malabo )
    , ( "Africa/Maputo", africa__maputo )
    , ( "Africa/Maseru", africa__maseru )
    , ( "Africa/Mbabane", africa__mbabane )
    , ( "Africa/Mogadishu", africa__mogadishu )
    , ( "Africa/Monrovia", africa__monrovia )
    , ( "Africa/Nairobi", africa__nairobi )
    , ( "Africa/Ndjamena", africa__ndjamena )
    , ( "Africa/Niamey", africa__niamey )
    , ( "Africa/Nouakchott", africa__nouakchott )
    , ( "Africa/Ouagadougou", africa__ouagadougou )
    , ( "Africa/Porto-Novo", africa__porto_novo )
    , ( "Africa/Sao_Tome", africa__sao_tome )
    , ( "Africa/Tripoli", africa__tripoli )
    , ( "Africa/Tunis", africa__tunis )
    , ( "Africa/Windhoek", africa__windhoek )
    , ( "America/Adak", america__adak )
    , ( "America/Anchorage", america__anchorage )
    , ( "America/Anguilla", america__anguilla )
    , ( "America/Antigua", america__antigua )
    , ( "America/Araguaina", america__araguaina )
    , ( "America/Argentina/Buenos_Aires", america__argentina__buenos_aires )
    , ( "America/Argentina/Catamarca", america__argentina__catamarca )
    , ( "America/Argentina/Cordoba", america__argentina__cordoba )
    , ( "America/Argentina/Jujuy", america__argentina__jujuy )
    , ( "America/Argentina/La_Rioja", america__argentina__la_rioja )
    , ( "America/Argentina/Mendoza", america__argentina__mendoza )
    , ( "America/Argentina/Rio_Gallegos", america__argentina__rio_gallegos )
    , ( "America/Argentina/Salta", america__argentina__salta )
    , ( "America/Argentina/San_Juan", america__argentina__san_juan )
    , ( "America/Argentina/San_Luis", america__argentina__san_luis )
    , ( "America/Argentina/Tucuman", america__argentina__tucuman )
    , ( "America/Argentina/Ushuaia", america__argentina__ushuaia )
    , ( "America/Aruba", america__aruba )
    , ( "America/Asuncion", america__asuncion )
    , ( "America/Atikokan", america__atikokan )
    , ( "America/Bahia", america__bahia )
    , ( "America/Bahia_Banderas", america__bahia_banderas )
    , ( "America/Barbados", america__barbados )
    , ( "America/Belem", america__belem )
    , ( "America/Belize", america__belize )
    , ( "America/Blanc-Sablon", america__blanc_sablon )
    , ( "America/Boa_Vista", america__boa_vista )
    , ( "America/Bogota", america__bogota )
    , ( "America/Boise", america__boise )
    , ( "America/Cambridge_Bay", america__cambridge_bay )
    , ( "America/Campo_Grande", america__campo_grande )
    , ( "America/Cancun", america__cancun )
    , ( "America/Caracas", america__caracas )
    , ( "America/Cayenne", america__cayenne )
    , ( "America/Cayman", america__cayman )
    , ( "America/Chicago", america__chicago )
    , ( "America/Chihuahua", america__chihuahua )
    , ( "America/Costa_Rica", america__costa_rica )
    , ( "America/Creston", america__creston )
    , ( "America/Cuiaba", america__cuiaba )
    , ( "America/Curacao", america__curacao )
    , ( "America/Danmarkshavn", america__danmarkshavn )
    , ( "America/Dawson", america__dawson )
    , ( "America/Dawson_Creek", america__dawson_creek )
    , ( "America/Denver", america__denver )
    , ( "America/Detroit", america__detroit )
    , ( "America/Dominica", america__dominica )
    , ( "America/Edmonton", america__edmonton )
    , ( "America/Eirunepe", america__eirunepe )
    , ( "America/El_Salvador", america__el_salvador )
    , ( "America/Fort_Nelson", america__fort_nelson )
    , ( "America/Fortaleza", america__fortaleza )
    , ( "America/Glace_Bay", america__glace_bay )
    , ( "America/Goose_Bay", america__goose_bay )
    , ( "America/Grand_Turk", america__grand_turk )
    , ( "America/Grenada", america__grenada )
    , ( "America/Guadeloupe", america__guadeloupe )
    , ( "America/Guatemala", america__guatemala )
    , ( "America/Guayaquil", america__guayaquil )
    , ( "America/Guyana", america__guyana )
    , ( "America/Halifax", america__halifax )
    , ( "America/Havana", america__havana )
    , ( "America/Hermosillo", america__hermosillo )
    , ( "America/Indiana/Indianapolis", america__indiana__indianapolis )
    , ( "America/Indiana/Knox", america__indiana__knox )
    , ( "America/Indiana/Marengo", america__indiana__marengo )
    , ( "America/Indiana/Petersburg", america__indiana__petersburg )
    , ( "America/Indiana/Tell_City", america__indiana__tell_city )
    , ( "America/Indiana/Vevay", america__indiana__vevay )
    , ( "America/Indiana/Vincennes", america__indiana__vincennes )
    , ( "America/Indiana/Winamac", america__indiana__winamac )
    , ( "America/Inuvik", america__inuvik )
    , ( "America/Iqaluit", america__iqaluit )
    , ( "America/Jamaica", america__jamaica )
    , ( "America/Juneau", america__juneau )
    , ( "America/Kentucky/Louisville", america__kentucky__louisville )
    , ( "America/Kentucky/Monticello", america__kentucky__monticello )
    , ( "America/Kralendijk", america__kralendijk )
    , ( "America/La_Paz", america__la_paz )
    , ( "America/Lima", america__lima )
    , ( "America/Los_Angeles", america__los_angeles )
    , ( "America/Lower_Princes", america__lower_princes )
    , ( "America/Maceio", america__maceio )
    , ( "America/Managua", america__managua )
    , ( "America/Manaus", america__manaus )
    , ( "America/Marigot", america__marigot )
    , ( "America/Martinique", america__martinique )
    , ( "America/Matamoros", america__matamoros )
    , ( "America/Mazatlan", america__mazatlan )
    , ( "America/Menominee", america__menominee )
    , ( "America/Merida", america__merida )
    , ( "America/Metlakatla", america__metlakatla )
    , ( "America/Mexico_City", america__mexico_city )
    , ( "America/Miquelon", america__miquelon )
    , ( "America/Moncton", america__moncton )
    , ( "America/Monterrey", america__monterrey )
    , ( "America/Montevideo", america__montevideo )
    , ( "America/Montserrat", america__montserrat )
    , ( "America/Nassau", america__nassau )
    , ( "America/New_York", america__new_york )
    , ( "America/Nipigon", america__nipigon )
    , ( "America/Nome", america__nome )
    , ( "America/Noronha", america__noronha )
    , ( "America/North_Dakota/Beulah", america__north_dakota__beulah )
    , ( "America/North_Dakota/Center", america__north_dakota__center )
    , ( "America/North_Dakota/New_Salem", america__north_dakota__new_salem )
    , ( "America/Nuuk", america__nuuk )
    , ( "America/Ojinaga", america__ojinaga )
    , ( "America/Panama", america__panama )
    , ( "America/Pangnirtung", america__pangnirtung )
    , ( "America/Paramaribo", america__paramaribo )
    , ( "America/Phoenix", america__phoenix )
    , ( "America/Port-au-Prince", america__port_au_prince )
    , ( "America/Port_of_Spain", america__port_of_spain )
    , ( "America/Porto_Velho", america__porto_velho )
    , ( "America/Puerto_Rico", america__puerto_rico )
    , ( "America/Punta_Arenas", america__punta_arenas )
    , ( "America/Rainy_River", america__rainy_river )
    , ( "America/Rankin_Inlet", america__rankin_inlet )
    , ( "America/Recife", america__recife )
    , ( "America/Regina", america__regina )
    , ( "America/Resolute", america__resolute )
    , ( "America/Rio_Branco", america__rio_branco )
    , ( "America/Santarem", america__santarem )
    , ( "America/Santiago", america__santiago )
    , ( "America/Santo_Domingo", america__santo_domingo )
    , ( "America/Sao_Paulo", america__sao_paulo )
    , ( "America/Scoresbysund", america__scoresbysund )
    , ( "America/Sitka", america__sitka )
    , ( "America/St_Barthelemy", america__st_barthelemy )
    , ( "America/St_Johns", america__st_johns )
    , ( "America/St_Kitts", america__st_kitts )
    , ( "America/St_Lucia", america__st_lucia )
    , ( "America/St_Thomas", america__st_thomas )
    , ( "America/St_Vincent", america__st_vincent )
    , ( "America/Swift_Current", america__swift_current )
    , ( "America/Tegucigalpa", america__tegucigalpa )
    , ( "America/Thule", america__thule )
    , ( "America/Thunder_Bay", america__thunder_bay )
    , ( "America/Tijuana", america__tijuana )
    , ( "America/Toronto", america__toronto )
    , ( "America/Tortola", america__tortola )
    , ( "America/Vancouver", america__vancouver )
    , ( "America/Whitehorse", america__whitehorse )
    , ( "America/Winnipeg", america__winnipeg )
    , ( "America/Yakutat", america__yakutat )
    , ( "America/Yellowknife", america__yellowknife )
    , ( "Antarctica/Casey", antarctica__casey )
    , ( "Antarctica/Davis", antarctica__davis )
    , ( "Antarctica/DumontDUrville", antarctica__dumontdurville )
    , ( "Antarctica/Macquarie", antarctica__macquarie )
    , ( "Antarctica/Mawson", antarctica__mawson )
    , ( "Antarctica/McMurdo", antarctica__mcmurdo )
    , ( "Antarctica/Palmer", antarctica__palmer )
    , ( "Antarctica/Rothera", antarctica__rothera )
    , ( "Antarctica/Syowa", antarctica__syowa )
    , ( "Antarctica/Troll", antarctica__troll )
    , ( "Antarctica/Vostok", antarctica__vostok )
    , ( "Arctic/Longyearbyen", arctic__longyearbyen )
    , ( "Asia/Aden", asia__aden )
    , ( "Asia/Almaty", asia__almaty )
    , ( "Asia/Amman", asia__amman )
    , ( "Asia/Anadyr", asia__anadyr )
    , ( "Asia/Aqtau", asia__aqtau )
    , ( "Asia/Aqtobe", asia__aqtobe )
    , ( "Asia/Ashgabat", asia__ashgabat )
    , ( "Asia/Atyrau", asia__atyrau )
    , ( "Asia/Baghdad", asia__baghdad )
    , ( "Asia/Bahrain", asia__bahrain )
    , ( "Asia/Baku", asia__baku )
    , ( "Asia/Bangkok", asia__bangkok )
    , ( "Asia/Barnaul", asia__barnaul )
    , ( "Asia/Beirut", asia__beirut )
    , ( "Asia/Bishkek", asia__bishkek )
    , ( "Asia/Brunei", asia__brunei )
    , ( "Asia/Chita", asia__chita )
    , ( "Asia/Choibalsan", asia__choibalsan )
    , ( "Asia/Colombo", asia__colombo )
    , ( "Asia/Damascus", asia__damascus )
    , ( "Asia/Dhaka", asia__dhaka )
    , ( "Asia/Dili", asia__dili )
    , ( "Asia/Dubai", asia__dubai )
    , ( "Asia/Dushanbe", asia__dushanbe )
    , ( "Asia/Famagusta", asia__famagusta )
    , ( "Asia/Gaza", asia__gaza )
    , ( "Asia/Hebron", asia__hebron )
    , ( "Asia/Ho_Chi_Minh", asia__ho_chi_minh )
    , ( "Asia/Hong_Kong", asia__hong_kong )
    , ( "Asia/Hovd", asia__hovd )
    , ( "Asia/Irkutsk", asia__irkutsk )
    , ( "Asia/Istanbul", asia__istanbul )
    , ( "Asia/Jakarta", asia__jakarta )
    , ( "Asia/Jayapura", asia__jayapura )
    , ( "Asia/Jerusalem", asia__jerusalem )
    , ( "Asia/Kabul", asia__kabul )
    , ( "Asia/Kamchatka", asia__kamchatka )
    , ( "Asia/Karachi", asia__karachi )
    , ( "Asia/Kathmandu", asia__kathmandu )
    , ( "Asia/Khandyga", asia__khandyga )
    , ( "Asia/Kolkata", asia__kolkata )
    , ( "Asia/Krasnoyarsk", asia__krasnoyarsk )
    , ( "Asia/Kuala_Lumpur", asia__kuala_lumpur )
    , ( "Asia/Kuching", asia__kuching )
    , ( "Asia/Kuwait", asia__kuwait )
    , ( "Asia/Macau", asia__macau )
    , ( "Asia/Magadan", asia__magadan )
    , ( "Asia/Makassar", asia__makassar )
    , ( "Asia/Manila", asia__manila )
    , ( "Asia/Muscat", asia__muscat )
    , ( "Asia/Nicosia", asia__nicosia )
    , ( "Asia/Novokuznetsk", asia__novokuznetsk )
    , ( "Asia/Novosibirsk", asia__novosibirsk )
    , ( "Asia/Omsk", asia__omsk )
    , ( "Asia/Oral", asia__oral )
    , ( "Asia/Phnom_Penh", asia__phnom_penh )
    , ( "Asia/Pontianak", asia__pontianak )
    , ( "Asia/Pyongyang", asia__pyongyang )
    , ( "Asia/Qatar", asia__qatar )
    , ( "Asia/Qostanay", asia__qostanay )
    , ( "Asia/Qyzylorda", asia__qyzylorda )
    , ( "Asia/Riyadh", asia__riyadh )
    , ( "Asia/Sakhalin", asia__sakhalin )
    , ( "Asia/Samarkand", asia__samarkand )
    , ( "Asia/Seoul", asia__seoul )
    , ( "Asia/Shanghai", asia__shanghai )
    , ( "Asia/Singapore", asia__singapore )
    , ( "Asia/Srednekolymsk", asia__srednekolymsk )
    , ( "Asia/Taipei", asia__taipei )
    , ( "Asia/Tashkent", asia__tashkent )
    , ( "Asia/Tbilisi", asia__tbilisi )
    , ( "Asia/Tehran", asia__tehran )
    , ( "Asia/Thimphu", asia__thimphu )
    , ( "Asia/Tokyo", asia__tokyo )
    , ( "Asia/Tomsk", asia__tomsk )
    , ( "Asia/Ulaanbaatar", asia__ulaanbaatar )
    , ( "Asia/Urumqi", asia__urumqi )
    , ( "Asia/Ust-Nera", asia__ust_nera )
    , ( "Asia/Vientiane", asia__vientiane )
    , ( "Asia/Vladivostok", asia__vladivostok )
    , ( "Asia/Yakutsk", asia__yakutsk )
    , ( "Asia/Yangon", asia__yangon )
    , ( "Asia/Yekaterinburg", asia__yekaterinburg )
    , ( "Asia/Yerevan", asia__yerevan )
    , ( "Atlantic/Azores", atlantic__azores )
    , ( "Atlantic/Bermuda", atlantic__bermuda )
    , ( "Atlantic/Canary", atlantic__canary )
    , ( "Atlantic/Cape_Verde", atlantic__cape_verde )
    , ( "Atlantic/Faroe", atlantic__faroe )
    , ( "Atlantic/Madeira", atlantic__madeira )
    , ( "Atlantic/Reykjavik", atlantic__reykjavik )
    , ( "Atlantic/South_Georgia", atlantic__south_georgia )
    , ( "Atlantic/St_Helena", atlantic__st_helena )
    , ( "Atlantic/Stanley", atlantic__stanley )
    , ( "Australia/Adelaide", australia__adelaide )
    , ( "Australia/Brisbane", australia__brisbane )
    , ( "Australia/Broken_Hill", australia__broken_hill )
    , ( "Australia/Darwin", australia__darwin )
    , ( "Australia/Eucla", australia__eucla )
    , ( "Australia/Hobart", australia__hobart )
    , ( "Australia/Lindeman", australia__lindeman )
    , ( "Australia/Lord_Howe", australia__lord_howe )
    , ( "Australia/Melbourne", australia__melbourne )
    , ( "Australia/Perth", australia__perth )
    , ( "Australia/Sydney", australia__sydney )
    , ( "Europe/Amsterdam", europe__amsterdam )
    , ( "Europe/Andorra", europe__andorra )
    , ( "Europe/Astrakhan", europe__astrakhan )
    , ( "Europe/Athens", europe__athens )
    , ( "Europe/Belgrade", europe__belgrade )
    , ( "Europe/Berlin", europe__berlin )
    , ( "Europe/Bratislava", europe__bratislava )
    , ( "Europe/Brussels", europe__brussels )
    , ( "Europe/Bucharest", europe__bucharest )
    , ( "Europe/Budapest", europe__budapest )
    , ( "Europe/Busingen", europe__busingen )
    , ( "Europe/Chisinau", europe__chisinau )
    , ( "Europe/Copenhagen", europe__copenhagen )
    , ( "Europe/Dublin", europe__dublin )
    , ( "Europe/Gibraltar", europe__gibraltar )
    , ( "Europe/Guernsey", europe__guernsey )
    , ( "Europe/Helsinki", europe__helsinki )
    , ( "Europe/Isle_of_Man", europe__isle_of_man )
    , ( "Europe/Istanbul", europe__istanbul )
    , ( "Europe/Jersey", europe__jersey )
    , ( "Europe/Kaliningrad", europe__kaliningrad )
    , ( "Europe/Kiev", europe__kiev )
    , ( "Europe/Kirov", europe__kirov )
    , ( "Europe/Lisbon", europe__lisbon )
    , ( "Europe/Ljubljana", europe__ljubljana )
    , ( "Europe/London", europe__london )
    , ( "Europe/Luxembourg", europe__luxembourg )
    , ( "Europe/Madrid", europe__madrid )
    , ( "Europe/Malta", europe__malta )
    , ( "Europe/Mariehamn", europe__mariehamn )
    , ( "Europe/Minsk", europe__minsk )
    , ( "Europe/Monaco", europe__monaco )
    , ( "Europe/Moscow", europe__moscow )
    , ( "Europe/Nicosia", europe__nicosia )
    , ( "Europe/Oslo", europe__oslo )
    , ( "Europe/Paris", europe__paris )
    , ( "Europe/Podgorica", europe__podgorica )
    , ( "Europe/Prague", europe__prague )
    , ( "Europe/Riga", europe__riga )
    , ( "Europe/Rome", europe__rome )
    , ( "Europe/Samara", europe__samara )
    , ( "Europe/San_Marino", europe__san_marino )
    , ( "Europe/Sarajevo", europe__sarajevo )
    , ( "Europe/Saratov", europe__saratov )
    , ( "Europe/Simferopol", europe__simferopol )
    , ( "Europe/Skopje", europe__skopje )
    , ( "Europe/Sofia", europe__sofia )
    , ( "Europe/Stockholm", europe__stockholm )
    , ( "Europe/Tallinn", europe__tallinn )
    , ( "Europe/Tirane", europe__tirane )
    , ( "Europe/Ulyanovsk", europe__ulyanovsk )
    , ( "Europe/Uzhgorod", europe__uzhgorod )
    , ( "Europe/Vaduz", europe__vaduz )
    , ( "Europe/Vatican", europe__vatican )
    , ( "Europe/Vienna", europe__vienna )
    , ( "Europe/Vilnius", europe__vilnius )
    , ( "Europe/Volgograd", europe__volgograd )
    , ( "Europe/Warsaw", europe__warsaw )
    , ( "Europe/Zagreb", europe__zagreb )
    , ( "Europe/Zaporozhye", europe__zaporozhye )
    , ( "Europe/Zurich", europe__zurich )
    , ( "Indian/Antananarivo", indian__antananarivo )
    , ( "Indian/Chagos", indian__chagos )
    , ( "Indian/Christmas", indian__christmas )
    , ( "Indian/Cocos", indian__cocos )
    , ( "Indian/Comoro", indian__comoro )
    , ( "Indian/Kerguelen", indian__kerguelen )
    , ( "Indian/Mahe", indian__mahe )
    , ( "Indian/Maldives", indian__maldives )
    , ( "Indian/Mauritius", indian__mauritius )
    , ( "Indian/Mayotte", indian__mayotte )
    , ( "Indian/Reunion", indian__reunion )
    , ( "Pacific/Apia", pacific__apia )
    , ( "Pacific/Auckland", pacific__auckland )
    , ( "Pacific/Bougainville", pacific__bougainville )
    , ( "Pacific/Chatham", pacific__chatham )
    , ( "Pacific/Chuuk", pacific__chuuk )
    , ( "Pacific/Easter", pacific__easter )
    , ( "Pacific/Efate", pacific__efate )
    , ( "Pacific/Fakaofo", pacific__fakaofo )
    , ( "Pacific/Fiji", pacific__fiji )
    , ( "Pacific/Funafuti", pacific__funafuti )
    , ( "Pacific/Galapagos", pacific__galapagos )
    , ( "Pacific/Gambier", pacific__gambier )
    , ( "Pacific/Guadalcanal", pacific__guadalcanal )
    , ( "Pacific/Guam", pacific__guam )
    , ( "Pacific/Honolulu", pacific__honolulu )
    , ( "Pacific/Kanton", pacific__kanton )
    , ( "Pacific/Kiritimati", pacific__kiritimati )
    , ( "Pacific/Kosrae", pacific__kosrae )
    , ( "Pacific/Kwajalein", pacific__kwajalein )
    , ( "Pacific/Majuro", pacific__majuro )
    , ( "Pacific/Marquesas", pacific__marquesas )
    , ( "Pacific/Midway", pacific__midway )
    , ( "Pacific/Nauru", pacific__nauru )
    , ( "Pacific/Niue", pacific__niue )
    , ( "Pacific/Norfolk", pacific__norfolk )
    , ( "Pacific/Noumea", pacific__noumea )
    , ( "Pacific/Pago_Pago", pacific__pago_pago )
    , ( "Pacific/Palau", pacific__palau )
    , ( "Pacific/Pitcairn", pacific__pitcairn )
    , ( "Pacific/Pohnpei", pacific__pohnpei )
    , ( "Pacific/Port_Moresby", pacific__port_moresby )
    , ( "Pacific/Rarotonga", pacific__rarotonga )
    , ( "Pacific/Saipan", pacific__saipan )
    , ( "Pacific/Tahiti", pacific__tahiti )
    , ( "Pacific/Tarawa", pacific__tarawa )
    , ( "Pacific/Tongatapu", pacific__tongatapu )
    , ( "Pacific/Wake", pacific__wake )
    , ( "Pacific/Wallis", pacific__wallis )
    ]
        |> Dict.fromList



-- Rules


rules_AN : List Rule
rules_AN =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Day 27) 120 Standard 0
    , Rule 1973 1981 Mar (Next Sun 1) 120 Standard 0
    , Rule 1982 1982 Apr (Next Sun 1) 120 Standard 0
    , Rule 1983 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1989 Mar (Next Sun 15) 120 Standard 0
    , Rule 1986 1986 Oct (Day 19) 120 Standard 60
    , Rule 1987 1999 Oct (Last Sun) 120 Standard 60
    , Rule 1990 1995 Mar (Next Sun 1) 120 Standard 0
    , Rule 1996 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 2007 Oct (Last Sun) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AQ : List Rule
rules_AQ =
    [ Rule 1971 1971 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1989 1991 Oct (Last Sun) 120 Standard 60
    , Rule 1990 1992 Mar (Next Sun 1) 120 Standard 0
    ]


rules_AS : List Rule
rules_AS =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1986 1986 Oct (Day 19) 120 Standard 60
    , Rule 1987 2007 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Day 27) 120 Standard 0
    , Rule 1973 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1991 1991 Mar (Day 3) 120 Standard 0
    , Rule 1992 1992 Mar (Day 22) 120 Standard 0
    , Rule 1993 1993 Mar (Day 7) 120 Standard 0
    , Rule 1994 1994 Mar (Day 20) 120 Standard 0
    , Rule 1995 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2006 2006 Apr (Day 2) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AT : List Rule
rules_AT =
    [ Rule 1968 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1969 1971 Mar (Next Sun 8) 120 Standard 0
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1973 1981 Mar (Next Sun 1) 120 Standard 0
    , Rule 1982 1983 Mar (Last Sun) 120 Standard 0
    , Rule 1984 1986 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1986 Oct (Next Sun 15) 120 Standard 60
    , Rule 1987 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1987 1987 Oct (Next Sun 22) 120 Standard 60
    , Rule 1988 1990 Oct (Last Sun) 120 Standard 60
    , Rule 1991 1999 Oct (Next Sun 1) 120 Standard 60
    , Rule 1991 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 maxYear Oct (Next Sun 1) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    ]


rules_AV : List Rule
rules_AV =
    [ Rule 1971 1985 Oct (Last Sun) 120 Standard 60
    , Rule 1972 1972 Feb (Last Sun) 120 Standard 0
    , Rule 1973 1985 Mar (Next Sun 1) 120 Standard 0
    , Rule 1986 1990 Mar (Next Sun 15) 120 Standard 0
    , Rule 1986 1987 Oct (Next Sun 15) 120 Standard 60
    , Rule 1988 1999 Oct (Last Sun) 120 Standard 60
    , Rule 1991 1994 Mar (Next Sun 1) 120 Standard 0
    , Rule 1995 2005 Mar (Last Sun) 120 Standard 0
    , Rule 2000 2000 Aug (Last Sun) 120 Standard 60
    , Rule 2001 2007 Oct (Last Sun) 120 Standard 60
    , Rule 2006 2006 Apr (Next Sun 1) 120 Standard 0
    , Rule 2007 2007 Mar (Last Sun) 120 Standard 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 Standard 60
    ]


rules_AW : List Rule
rules_AW =
    [ Rule 1974 1974 Oct (Last Sun) 120 Standard 60
    , Rule 1975 1975 Mar (Next Sun 1) 120 Standard 0
    , Rule 1983 1983 Oct (Last Sun) 120 Standard 60
    , Rule 1984 1984 Mar (Next Sun 1) 120 Standard 0
    , Rule 1991 1991 Nov (Day 17) 120 Standard 60
    , Rule 1992 1992 Mar (Next Sun 1) 120 Standard 0
    , Rule 2006 2006 Dec (Day 3) 120 Standard 60
    , Rule 2007 2009 Mar (Last Sun) 120 Standard 0
    , Rule 2007 2008 Oct (Last Sun) 120 Standard 60
    ]


rules_Albania : List Rule
rules_Albania =
    [ Rule 1974 1974 May (Day 4) 0 WallClock 60
    , Rule 1974 1974 Oct (Day 2) 0 WallClock 0
    , Rule 1975 1975 May (Day 1) 0 WallClock 60
    , Rule 1975 1975 Oct (Day 2) 0 WallClock 0
    , Rule 1976 1976 May (Day 2) 0 WallClock 60
    , Rule 1976 1976 Oct (Day 3) 0 WallClock 0
    , Rule 1977 1977 May (Day 8) 0 WallClock 60
    , Rule 1977 1977 Oct (Day 2) 0 WallClock 0
    , Rule 1978 1978 May (Day 6) 0 WallClock 60
    , Rule 1978 1978 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1979 May (Day 5) 0 WallClock 60
    , Rule 1979 1979 Sep (Day 30) 0 WallClock 0
    , Rule 1980 1980 May (Day 3) 0 WallClock 60
    , Rule 1980 1980 Oct (Day 4) 0 WallClock 0
    , Rule 1981 1981 Apr (Day 26) 0 WallClock 60
    , Rule 1981 1981 Sep (Day 27) 0 WallClock 0
    , Rule 1982 1982 May (Day 2) 0 WallClock 60
    , Rule 1982 1982 Oct (Day 3) 0 WallClock 0
    , Rule 1983 1983 Apr (Day 18) 0 WallClock 60
    , Rule 1983 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1984 Apr (Day 1) 0 WallClock 60
    ]


rules_Algeria : List Rule
rules_Algeria =
    [ Rule 1971 1971 Apr (Day 25) 1380 Standard 60
    , Rule 1971 1971 Sep (Day 26) 1380 Standard 0
    , Rule 1977 1977 May (Day 6) 0 WallClock 60
    , Rule 1977 1977 Oct (Day 21) 0 WallClock 0
    , Rule 1978 1978 Mar (Day 24) 60 WallClock 60
    , Rule 1978 1978 Sep (Day 22) 180 WallClock 0
    , Rule 1980 1980 Apr (Day 25) 0 WallClock 60
    , Rule 1980 1980 Oct (Day 31) 120 WallClock 0
    ]


rules_Arg : List Rule
rules_Arg =
    [ Rule 1968 1969 Apr (Next Sun 1) 0 WallClock 0
    , Rule 1974 1974 Jan (Day 23) 0 WallClock 60
    , Rule 1974 1974 May (Day 1) 0 WallClock 0
    , Rule 1988 1988 Dec (Day 1) 0 WallClock 60
    , Rule 1989 1993 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1989 1992 Oct (Next Sun 15) 0 WallClock 60
    , Rule 1999 1999 Oct (Next Sun 1) 0 WallClock 60
    , Rule 2000 2000 Mar (Day 3) 0 WallClock 0
    , Rule 2007 2007 Dec (Day 30) 0 WallClock 60
    , Rule 2008 2009 Mar (Next Sun 15) 0 WallClock 0
    , Rule 2008 2008 Oct (Next Sun 15) 0 WallClock 60
    ]


rules_Armenia : List Rule
rules_Armenia =
    [ Rule 2011 2011 Mar (Last Sun) 120 Standard 60
    , Rule 2011 2011 Oct (Last Sun) 120 Standard 0
    ]


rules_Austria : List Rule
rules_Austria =
    [ Rule 1980 1980 Apr (Day 6) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 0 WallClock 0
    ]


rules_Azer : List Rule
rules_Azer =
    [ Rule 1997 2015 Mar (Last Sun) 240 WallClock 60
    , Rule 1997 2015 Oct (Last Sun) 300 WallClock 0
    ]


rules_Barb : List Rule
rules_Barb =
    [ Rule 1977 1977 Jun (Day 12) 120 WallClock 60
    , Rule 1977 1978 Oct (Next Sun 1) 120 WallClock 0
    , Rule 1978 1980 Apr (Next Sun 15) 120 WallClock 60
    , Rule 1979 1979 Sep (Day 30) 120 WallClock 0
    , Rule 1980 1980 Sep (Day 25) 120 WallClock 0
    ]


rules_Belize : List Rule
rules_Belize =
    [ Rule 1973 1973 Dec (Day 5) 0 WallClock 60
    , Rule 1974 1974 Feb (Day 9) 0 WallClock 0
    , Rule 1982 1982 Dec (Day 18) 0 WallClock 60
    , Rule 1983 1983 Feb (Day 12) 0 WallClock 0
    ]


rules_Brazil : List Rule
rules_Brazil =
    [ Rule 1985 1985 Nov (Day 2) 0 WallClock 60
    , Rule 1986 1986 Mar (Day 15) 0 WallClock 0
    , Rule 1986 1986 Oct (Day 25) 0 WallClock 60
    , Rule 1987 1987 Feb (Day 14) 0 WallClock 0
    , Rule 1987 1987 Oct (Day 25) 0 WallClock 60
    , Rule 1988 1988 Feb (Day 7) 0 WallClock 0
    , Rule 1988 1988 Oct (Day 16) 0 WallClock 60
    , Rule 1989 1989 Jan (Day 29) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 15) 0 WallClock 60
    , Rule 1990 1990 Feb (Day 11) 0 WallClock 0
    , Rule 1990 1990 Oct (Day 21) 0 WallClock 60
    , Rule 1991 1991 Feb (Day 17) 0 WallClock 0
    , Rule 1991 1991 Oct (Day 20) 0 WallClock 60
    , Rule 1992 1992 Feb (Day 9) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 25) 0 WallClock 60
    , Rule 1993 1993 Jan (Day 31) 0 WallClock 0
    , Rule 1993 1995 Oct (Next Sun 11) 0 WallClock 60
    , Rule 1994 1995 Feb (Next Sun 15) 0 WallClock 0
    , Rule 1996 1996 Feb (Day 11) 0 WallClock 0
    , Rule 1996 1996 Oct (Day 6) 0 WallClock 60
    , Rule 1997 1997 Feb (Day 16) 0 WallClock 0
    , Rule 1997 1997 Oct (Day 6) 0 WallClock 60
    , Rule 1998 1998 Mar (Day 1) 0 WallClock 0
    , Rule 1998 1998 Oct (Day 11) 0 WallClock 60
    , Rule 1999 1999 Feb (Day 21) 0 WallClock 0
    , Rule 1999 1999 Oct (Day 3) 0 WallClock 60
    , Rule 2000 2000 Feb (Day 27) 0 WallClock 0
    , Rule 2000 2001 Oct (Next Sun 8) 0 WallClock 60
    , Rule 2001 2006 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2002 2002 Nov (Day 3) 0 WallClock 60
    , Rule 2003 2003 Oct (Day 19) 0 WallClock 60
    , Rule 2004 2004 Nov (Day 2) 0 WallClock 60
    , Rule 2005 2005 Oct (Day 16) 0 WallClock 60
    , Rule 2006 2006 Nov (Day 5) 0 WallClock 60
    , Rule 2007 2007 Feb (Day 25) 0 WallClock 0
    , Rule 2007 2007 Oct (Next Sun 8) 0 WallClock 60
    , Rule 2008 2017 Oct (Next Sun 15) 0 WallClock 60
    , Rule 2008 2011 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2012 2012 Feb (Next Sun 22) 0 WallClock 0
    , Rule 2013 2014 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2015 2015 Feb (Next Sun 22) 0 WallClock 0
    , Rule 2016 2019 Feb (Next Sun 15) 0 WallClock 0
    , Rule 2018 2018 Nov (Next Sun 1) 0 WallClock 60
    ]


rules_Bulg : List Rule
rules_Bulg =
    [ Rule 1979 1979 Mar (Day 31) 1380 WallClock 60
    , Rule 1979 1979 Oct (Day 1) 60 WallClock 0
    , Rule 1980 1982 Apr (Next Sat 1) 1380 WallClock 60
    , Rule 1980 1980 Sep (Day 29) 60 WallClock 0
    , Rule 1981 1981 Sep (Day 27) 120 WallClock 0
    ]


rules_C_Eur : List Rule
rules_C_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 120 Standard 60
    , Rule 1977 1977 Sep (Last Sun) 120 Standard 0
    , Rule 1978 1978 Oct (Day 1) 120 Standard 0
    , Rule 1979 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1981 maxYear Mar (Last Sun) 120 Standard 60
    , Rule 1996 maxYear Oct (Last Sun) 120 Standard 0
    ]


rules_CO : List Rule
rules_CO =
    [ Rule 1992 1992 May (Day 3) 0 WallClock 60
    , Rule 1993 1993 Apr (Day 4) 0 WallClock 0
    ]


rules_CR : List Rule
rules_CR =
    [ Rule 1979 1980 Feb (Last Sun) 0 WallClock 60
    , Rule 1979 1980 Jun (Next Sun 1) 0 WallClock 0
    , Rule 1991 1992 Jan (Next Sat 15) 0 WallClock 60
    , Rule 1991 1991 Jul (Day 1) 0 WallClock 0
    , Rule 1992 1992 Mar (Day 15) 0 WallClock 0
    ]


rules_Canada : List Rule
rules_Canada =
    [ Rule 1974 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1974 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Chatham : List Rule
rules_Chatham =
    [ Rule 1974 1974 Nov (Next Sun 1) 165 Standard 60
    , Rule 1975 1975 Feb (Last Sun) 165 Standard 0
    , Rule 1975 1988 Oct (Last Sun) 165 Standard 60
    , Rule 1976 1989 Mar (Next Sun 1) 165 Standard 0
    , Rule 1989 1989 Oct (Next Sun 8) 165 Standard 60
    , Rule 1990 2006 Oct (Next Sun 1) 165 Standard 60
    , Rule 1990 2007 Mar (Next Sun 15) 165 Standard 0
    , Rule 2007 maxYear Sep (Last Sun) 165 Standard 60
    , Rule 2008 maxYear Apr (Next Sun 1) 165 Standard 0
    ]


rules_Chile : List Rule
rules_Chile =
    [ Rule 1969 1969 Mar (Day 30) 180 Universal 0
    , Rule 1969 1969 Nov (Day 23) 240 Universal 60
    , Rule 1970 1970 Mar (Day 29) 180 Universal 0
    , Rule 1971 1971 Mar (Day 14) 180 Universal 0
    , Rule 1970 1972 Oct (Next Sun 9) 240 Universal 60
    , Rule 1972 1986 Mar (Next Sun 9) 180 Universal 0
    , Rule 1973 1973 Sep (Day 30) 240 Universal 60
    , Rule 1974 1987 Oct (Next Sun 9) 240 Universal 60
    , Rule 1987 1987 Apr (Day 12) 180 Universal 0
    , Rule 1988 1990 Mar (Next Sun 9) 180 Universal 0
    , Rule 1988 1989 Oct (Next Sun 9) 240 Universal 60
    , Rule 1990 1990 Sep (Day 16) 240 Universal 60
    , Rule 1991 1996 Mar (Next Sun 9) 180 Universal 0
    , Rule 1991 1997 Oct (Next Sun 9) 240 Universal 60
    , Rule 1997 1997 Mar (Day 30) 180 Universal 0
    , Rule 1998 1998 Mar (Next Sun 9) 180 Universal 0
    , Rule 1998 1998 Sep (Day 27) 240 Universal 60
    , Rule 1999 1999 Apr (Day 4) 180 Universal 0
    , Rule 1999 2010 Oct (Next Sun 9) 240 Universal 60
    , Rule 2000 2007 Mar (Next Sun 9) 180 Universal 0
    , Rule 2008 2008 Mar (Day 30) 180 Universal 0
    , Rule 2009 2009 Mar (Next Sun 9) 180 Universal 0
    , Rule 2010 2010 Apr (Next Sun 1) 180 Universal 0
    , Rule 2011 2011 May (Next Sun 2) 180 Universal 0
    , Rule 2011 2011 Aug (Next Sun 16) 240 Universal 60
    , Rule 2012 2014 Apr (Next Sun 23) 180 Universal 0
    , Rule 2012 2014 Sep (Next Sun 2) 240 Universal 60
    , Rule 2016 2018 May (Next Sun 9) 180 Universal 0
    , Rule 2016 2018 Aug (Next Sun 9) 240 Universal 60
    , Rule 2019 maxYear Apr (Next Sun 2) 180 Universal 0
    , Rule 2019 maxYear Sep (Next Sun 2) 240 Universal 60
    ]


rules_Cook : List Rule
rules_Cook =
    [ Rule 1978 1978 Nov (Day 12) 0 WallClock 30
    , Rule 1979 1991 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1979 1990 Oct (Last Sun) 0 WallClock 30
    ]


rules_Cuba : List Rule
rules_Cuba =
    [ Rule 1969 1977 Apr (Last Sun) 0 WallClock 60
    , Rule 1969 1971 Oct (Last Sun) 0 WallClock 0
    , Rule 1972 1974 Oct (Day 8) 0 WallClock 0
    , Rule 1975 1977 Oct (Last Sun) 0 WallClock 0
    , Rule 1978 1978 May (Day 7) 0 WallClock 60
    , Rule 1978 1990 Oct (Next Sun 8) 0 WallClock 0
    , Rule 1979 1980 Mar (Next Sun 15) 0 WallClock 60
    , Rule 1981 1985 May (Next Sun 5) 0 WallClock 60
    , Rule 1986 1989 Mar (Next Sun 14) 0 WallClock 60
    , Rule 1990 1997 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1991 1995 Oct (Next Sun 8) 0 Standard 0
    , Rule 1996 1996 Oct (Day 6) 0 Standard 0
    , Rule 1997 1997 Oct (Day 12) 0 Standard 0
    , Rule 1998 1999 Mar (Last Sun) 0 Standard 60
    , Rule 1998 2003 Oct (Last Sun) 0 Standard 0
    , Rule 2000 2003 Apr (Next Sun 1) 0 Standard 60
    , Rule 2004 2004 Mar (Last Sun) 0 Standard 60
    , Rule 2006 2010 Oct (Last Sun) 0 Standard 0
    , Rule 2007 2007 Mar (Next Sun 8) 0 Standard 60
    , Rule 2008 2008 Mar (Next Sun 15) 0 Standard 60
    , Rule 2009 2010 Mar (Next Sun 8) 0 Standard 60
    , Rule 2011 2011 Mar (Next Sun 15) 0 Standard 60
    , Rule 2011 2011 Nov (Day 13) 0 Standard 0
    , Rule 2012 2012 Apr (Day 1) 0 Standard 60
    , Rule 2012 maxYear Nov (Next Sun 1) 0 Standard 0
    , Rule 2013 maxYear Mar (Next Sun 8) 0 Standard 60
    ]


rules_Cyprus : List Rule
rules_Cyprus =
    [ Rule 1975 1975 Apr (Day 13) 0 WallClock 60
    , Rule 1975 1975 Oct (Day 12) 0 WallClock 0
    , Rule 1976 1976 May (Day 15) 0 WallClock 60
    , Rule 1976 1976 Oct (Day 11) 0 WallClock 0
    , Rule 1977 1980 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1977 1977 Sep (Day 25) 0 WallClock 0
    , Rule 1978 1978 Oct (Day 2) 0 WallClock 0
    , Rule 1979 1997 Sep (Last Sun) 0 WallClock 0
    , Rule 1981 1998 Mar (Last Sun) 0 WallClock 60
    ]


rules_DR : List Rule
rules_DR =
    [ Rule 1969 1973 Oct (Last Sun) 0 WallClock 30
    , Rule 1970 1970 Feb (Day 21) 0 WallClock 0
    , Rule 1971 1971 Jan (Day 20) 0 WallClock 0
    , Rule 1972 1974 Jan (Day 21) 0 WallClock 0
    ]


rules_Dhaka : List Rule
rules_Dhaka =
    [ Rule 2009 2009 Jun (Day 19) 1380 WallClock 60
    , Rule 2009 2009 Dec (Day 31) 1440 WallClock 0
    ]


rules_E_Eur : List Rule
rules_E_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 0 WallClock 60
    , Rule 1977 1977 Sep (Last Sun) 0 WallClock 0
    , Rule 1978 1978 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1995 Sep (Last Sun) 0 WallClock 0
    , Rule 1981 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1996 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_E_EurAsia : List Rule
rules_E_EurAsia =
    [ Rule 1981 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1979 1995 Sep (Last Sun) 0 WallClock 0
    , Rule 1996 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_EU : List Rule
rules_EU =
    [ Rule 1977 1980 Apr (Next Sun 1) 60 Universal 60
    , Rule 1977 1977 Sep (Last Sun) 60 Universal 0
    , Rule 1978 1978 Oct (Day 1) 60 Universal 0
    , Rule 1979 1995 Sep (Last Sun) 60 Universal 0
    , Rule 1981 maxYear Mar (Last Sun) 60 Universal 60
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_EUAsia : List Rule
rules_EUAsia =
    [ Rule 1981 maxYear Mar (Last Sun) 60 Universal 60
    , Rule 1979 1995 Sep (Last Sun) 60 Universal 0
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_Ecuador : List Rule
rules_Ecuador =
    [ Rule 1992 1992 Nov (Day 28) 0 WallClock 60
    , Rule 1993 1993 Feb (Day 5) 0 WallClock 0
    ]


rules_Edm : List Rule
rules_Edm =
    [ Rule 1972 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1972 2006 Oct (Last Sun) 120 WallClock 0
    ]


rules_Egypt : List Rule
rules_Egypt =
    [ Rule 1959 1981 May (Day 1) 60 WallClock 60
    , Rule 1966 1994 Oct (Day 1) 180 WallClock 0
    , Rule 1982 1982 Jul (Day 25) 60 WallClock 60
    , Rule 1983 1983 Jul (Day 12) 60 WallClock 60
    , Rule 1984 1988 May (Day 1) 60 WallClock 60
    , Rule 1989 1989 May (Day 6) 60 WallClock 60
    , Rule 1990 1994 May (Day 1) 60 WallClock 60
    , Rule 1995 2010 Apr (Last Fri) 0 Standard 60
    , Rule 1995 2005 Sep (Last Thu) 1440 WallClock 0
    , Rule 2006 2006 Sep (Day 21) 1440 WallClock 0
    , Rule 2007 2007 Sep (Next Thu 1) 1440 WallClock 0
    , Rule 2008 2008 Aug (Last Thu) 1440 WallClock 0
    , Rule 2009 2009 Aug (Day 20) 1440 WallClock 0
    , Rule 2010 2010 Aug (Day 10) 1440 WallClock 0
    , Rule 2010 2010 Sep (Day 9) 1440 WallClock 60
    , Rule 2010 2010 Sep (Last Thu) 1440 WallClock 0
    , Rule 2014 2014 May (Day 15) 1440 WallClock 60
    , Rule 2014 2014 Jun (Day 26) 1440 WallClock 0
    , Rule 2014 2014 Jul (Day 31) 1440 WallClock 60
    , Rule 2014 2014 Sep (Last Thu) 1440 WallClock 0
    ]


rules_Eire : List Rule
rules_Eire =
    [ Rule 1971 1971 Oct (Day 31) 120 Universal -60
    , Rule 1972 1980 Mar (Next Sun 16) 120 Universal 0
    , Rule 1972 1980 Oct (Next Sun 23) 120 Universal -60
    , Rule 1981 maxYear Mar (Last Sun) 60 Universal 0
    , Rule 1981 1989 Oct (Next Sun 23) 60 Universal -60
    , Rule 1990 1995 Oct (Next Sun 22) 60 Universal -60
    , Rule 1996 maxYear Oct (Last Sun) 60 Universal -60
    ]


rules_Falk : List Rule
rules_Falk =
    [ Rule 1983 1983 Sep (Last Sun) 0 WallClock 60
    , Rule 1984 1985 Apr (Last Sun) 0 WallClock 0
    , Rule 1984 1984 Sep (Day 16) 0 WallClock 60
    , Rule 1985 2000 Sep (Next Sun 9) 0 WallClock 60
    , Rule 1986 2000 Apr (Next Sun 16) 0 WallClock 0
    , Rule 2001 2010 Apr (Next Sun 15) 120 WallClock 0
    , Rule 2001 2010 Sep (Next Sun 1) 120 WallClock 60
    ]


rules_Fiji : List Rule
rules_Fiji =
    [ Rule 1998 1999 Nov (Next Sun 1) 120 WallClock 60
    , Rule 1999 2000 Feb (Last Sun) 180 WallClock 0
    , Rule 2009 2009 Nov (Day 29) 120 WallClock 60
    , Rule 2010 2010 Mar (Last Sun) 180 WallClock 0
    , Rule 2010 2013 Oct (Next Sun 21) 120 WallClock 60
    , Rule 2011 2011 Mar (Next Sun 1) 180 WallClock 0
    , Rule 2012 2013 Jan (Next Sun 18) 180 WallClock 0
    , Rule 2014 2014 Jan (Next Sun 18) 120 WallClock 0
    , Rule 2014 2018 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2015 2021 Jan (Next Sun 12) 180 WallClock 0
    , Rule 2019 2019 Nov (Next Sun 8) 120 WallClock 60
    , Rule 2020 2020 Dec (Day 20) 120 WallClock 60
    , Rule 2022 maxYear Nov (Next Sun 8) 120 WallClock 60
    , Rule 2023 maxYear Jan (Next Sun 12) 180 WallClock 0
    ]


rules_Finland : List Rule
rules_Finland =
    [ Rule 1981 1982 Mar (Last Sun) 120 WallClock 60
    , Rule 1981 1982 Sep (Last Sun) 180 WallClock 0
    ]


rules_France : List Rule
rules_France =
    [ Rule 1976 1976 Mar (Day 28) 60 WallClock 60
    , Rule 1976 1976 Sep (Day 26) 60 WallClock 0
    ]


rules_GB_Eire : List Rule
rules_GB_Eire =
    [ Rule 1972 1980 Mar (Next Sun 16) 120 Standard 60
    , Rule 1972 1980 Oct (Next Sun 23) 120 Standard 0
    , Rule 1981 1995 Mar (Last Sun) 60 Universal 60
    , Rule 1981 1989 Oct (Next Sun 23) 60 Universal 0
    , Rule 1990 1995 Oct (Next Sun 22) 60 Universal 0
    ]


rules_Greece : List Rule
rules_Greece =
    [ Rule 1975 1975 Apr (Day 12) 0 Standard 60
    , Rule 1975 1975 Nov (Day 26) 0 Standard 0
    , Rule 1976 1976 Apr (Day 11) 120 Standard 60
    , Rule 1976 1976 Oct (Day 10) 120 Standard 0
    , Rule 1977 1978 Apr (Next Sun 1) 120 Standard 60
    , Rule 1977 1977 Sep (Day 26) 120 Standard 0
    , Rule 1978 1978 Sep (Day 24) 240 WallClock 0
    , Rule 1979 1979 Apr (Day 1) 540 WallClock 60
    , Rule 1979 1979 Sep (Day 29) 120 WallClock 0
    , Rule 1980 1980 Apr (Day 1) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 0 WallClock 0
    ]


rules_Guam : List Rule
rules_Guam =
    [ Rule 1969 1969 Jan (Day 26) 1 WallClock 0
    , Rule 1969 1969 Jun (Day 22) 120 WallClock 60
    , Rule 1969 1969 Aug (Day 31) 120 WallClock 0
    , Rule 1970 1971 Apr (Last Sun) 120 WallClock 60
    , Rule 1970 1971 Sep (Next Sun 1) 120 WallClock 0
    , Rule 1973 1973 Dec (Day 16) 120 WallClock 60
    , Rule 1974 1974 Feb (Day 24) 120 WallClock 0
    , Rule 1976 1976 May (Day 26) 120 WallClock 60
    , Rule 1976 1976 Aug (Day 22) 121 WallClock 0
    , Rule 1977 1977 Apr (Day 24) 120 WallClock 60
    , Rule 1977 1977 Aug (Day 28) 120 WallClock 0
    ]


rules_Guat : List Rule
rules_Guat =
    [ Rule 1973 1973 Nov (Day 25) 0 WallClock 60
    , Rule 1974 1974 Feb (Day 24) 0 WallClock 0
    , Rule 1983 1983 May (Day 21) 0 WallClock 60
    , Rule 1983 1983 Sep (Day 22) 0 WallClock 0
    , Rule 1991 1991 Mar (Day 23) 0 WallClock 60
    , Rule 1991 1991 Sep (Day 7) 0 WallClock 0
    , Rule 2006 2006 Apr (Day 30) 0 WallClock 60
    , Rule 2006 2006 Oct (Day 1) 0 WallClock 0
    ]


rules_HK : List Rule
rules_HK =
    [ Rule 1965 1976 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1965 1976 Oct (Next Sun 16) 210 WallClock 0
    , Rule 1973 1973 Dec (Day 30) 210 WallClock 60
    , Rule 1979 1979 May (Day 13) 210 WallClock 60
    , Rule 1979 1979 Oct (Day 21) 210 WallClock 0
    ]


rules_Haiti : List Rule
rules_Haiti =
    [ Rule 1983 1983 May (Day 8) 0 WallClock 60
    , Rule 1984 1987 Apr (Last Sun) 0 WallClock 60
    , Rule 1983 1987 Oct (Last Sun) 0 WallClock 0
    , Rule 1988 1997 Apr (Next Sun 1) 60 Standard 60
    , Rule 1988 1997 Oct (Last Sun) 60 Standard 0
    , Rule 2005 2006 Apr (Next Sun 1) 0 WallClock 60
    , Rule 2005 2006 Oct (Last Sun) 0 WallClock 0
    , Rule 2012 2015 Mar (Next Sun 8) 120 WallClock 60
    , Rule 2012 2015 Nov (Next Sun 1) 120 WallClock 0
    , Rule 2017 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2017 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Halifax : List Rule
rules_Halifax =
    [ Rule 1962 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1962 1973 Oct (Last Sun) 120 WallClock 0
    ]


rules_Holiday : List Rule
rules_Holiday =
    [ Rule 1992 1993 Oct (Last Sun) 120 Standard 60
    , Rule 1993 1994 Mar (Next Sun 1) 120 Standard 0
    ]


rules_Hond : List Rule
rules_Hond =
    [ Rule 1987 1988 May (Next Sun 1) 0 WallClock 60
    , Rule 1987 1988 Sep (Last Sun) 0 WallClock 0
    , Rule 2006 2006 May (Next Sun 1) 0 WallClock 60
    , Rule 2006 2006 Aug (Next Mon 1) 0 WallClock 0
    ]


rules_Hungary : List Rule
rules_Hungary =
    [ Rule 1980 1980 Apr (Day 6) 0 WallClock 60
    , Rule 1980 1980 Sep (Day 28) 60 WallClock 0
    , Rule 1981 1983 Mar (Last Sun) 0 WallClock 60
    , Rule 1981 1983 Sep (Last Sun) 60 WallClock 0
    ]


rules_Iran : List Rule
rules_Iran =
    [ Rule 1978 1980 Mar (Day 20) 1440 WallClock 60
    , Rule 1978 1978 Oct (Day 20) 1440 WallClock 0
    , Rule 1979 1979 Sep (Day 18) 1440 WallClock 0
    , Rule 1980 1980 Sep (Day 22) 1440 WallClock 0
    , Rule 1991 1991 May (Day 2) 1440 WallClock 60
    , Rule 1992 1995 Mar (Day 21) 1440 WallClock 60
    , Rule 1991 1995 Sep (Day 21) 1440 WallClock 0
    , Rule 1996 1996 Mar (Day 20) 1440 WallClock 60
    , Rule 1996 1996 Sep (Day 20) 1440 WallClock 0
    , Rule 1997 1999 Mar (Day 21) 1440 WallClock 60
    , Rule 1997 1999 Sep (Day 21) 1440 WallClock 0
    , Rule 2000 2000 Mar (Day 20) 1440 WallClock 60
    , Rule 2000 2000 Sep (Day 20) 1440 WallClock 0
    , Rule 2001 2003 Mar (Day 21) 1440 WallClock 60
    , Rule 2001 2003 Sep (Day 21) 1440 WallClock 0
    , Rule 2004 2004 Mar (Day 20) 1440 WallClock 60
    , Rule 2004 2004 Sep (Day 20) 1440 WallClock 0
    , Rule 2005 2005 Mar (Day 21) 1440 WallClock 60
    , Rule 2005 2005 Sep (Day 21) 1440 WallClock 0
    , Rule 2008 2008 Mar (Day 20) 1440 WallClock 60
    , Rule 2008 2008 Sep (Day 20) 1440 WallClock 0
    , Rule 2009 2011 Mar (Day 21) 1440 WallClock 60
    , Rule 2009 2011 Sep (Day 21) 1440 WallClock 0
    , Rule 2012 2012 Mar (Day 20) 1440 WallClock 60
    , Rule 2012 2012 Sep (Day 20) 1440 WallClock 0
    , Rule 2013 2015 Mar (Day 21) 1440 WallClock 60
    , Rule 2013 2015 Sep (Day 21) 1440 WallClock 0
    , Rule 2016 2016 Mar (Day 20) 1440 WallClock 60
    , Rule 2016 2016 Sep (Day 20) 1440 WallClock 0
    , Rule 2017 2019 Mar (Day 21) 1440 WallClock 60
    , Rule 2017 2019 Sep (Day 21) 1440 WallClock 0
    , Rule 2020 2020 Mar (Day 20) 1440 WallClock 60
    , Rule 2020 2020 Sep (Day 20) 1440 WallClock 0
    , Rule 2021 2023 Mar (Day 21) 1440 WallClock 60
    , Rule 2021 2023 Sep (Day 21) 1440 WallClock 0
    , Rule 2024 2024 Mar (Day 20) 1440 WallClock 60
    , Rule 2024 2024 Sep (Day 20) 1440 WallClock 0
    , Rule 2025 2027 Mar (Day 21) 1440 WallClock 60
    , Rule 2025 2027 Sep (Day 21) 1440 WallClock 0
    , Rule 2028 2029 Mar (Day 20) 1440 WallClock 60
    , Rule 2028 2029 Sep (Day 20) 1440 WallClock 0
    , Rule 2030 2031 Mar (Day 21) 1440 WallClock 60
    , Rule 2030 2031 Sep (Day 21) 1440 WallClock 0
    , Rule 2032 2033 Mar (Day 20) 1440 WallClock 60
    , Rule 2032 2033 Sep (Day 20) 1440 WallClock 0
    , Rule 2034 2035 Mar (Day 21) 1440 WallClock 60
    , Rule 2034 2035 Sep (Day 21) 1440 WallClock 0
    , Rule 2036 2037 Mar (Day 20) 1440 WallClock 60
    , Rule 2036 2037 Sep (Day 20) 1440 WallClock 0
    ]


rules_Iraq : List Rule
rules_Iraq =
    [ Rule 1982 1982 May (Day 1) 0 WallClock 60
    , Rule 1982 1984 Oct (Day 1) 0 WallClock 0
    , Rule 1983 1983 Mar (Day 31) 0 WallClock 60
    , Rule 1984 1985 Apr (Day 1) 0 WallClock 60
    , Rule 1985 1990 Sep (Last Sun) 60 Standard 0
    , Rule 1986 1990 Mar (Last Sun) 60 Standard 60
    , Rule 1991 2007 Apr (Day 1) 180 Standard 60
    , Rule 1991 2007 Oct (Day 1) 180 Standard 0
    ]


rules_Italy : List Rule
rules_Italy =
    [ Rule 1967 1969 Sep (Next Sun 22) 0 Standard 0
    , Rule 1969 1969 Jun (Day 1) 0 Standard 60
    , Rule 1970 1970 May (Day 31) 0 Standard 60
    , Rule 1970 1970 Sep (Last Sun) 0 Standard 0
    , Rule 1971 1972 May (Next Sun 22) 0 Standard 60
    , Rule 1971 1971 Sep (Last Sun) 0 Standard 0
    , Rule 1972 1972 Oct (Day 1) 0 Standard 0
    , Rule 1973 1973 Jun (Day 3) 0 Standard 60
    , Rule 1973 1974 Sep (Last Sun) 0 Standard 0
    , Rule 1974 1974 May (Day 26) 0 Standard 60
    , Rule 1975 1975 Jun (Day 1) 0 Standard 60
    , Rule 1975 1977 Sep (Last Sun) 0 Standard 0
    , Rule 1976 1976 May (Day 30) 0 Standard 60
    , Rule 1977 1979 May (Next Sun 22) 0 Standard 60
    , Rule 1978 1978 Oct (Day 1) 0 Standard 0
    , Rule 1979 1979 Sep (Day 30) 0 Standard 0
    ]


rules_Jordan : List Rule
rules_Jordan =
    [ Rule 1973 1973 Jun (Day 6) 0 WallClock 60
    , Rule 1973 1975 Oct (Day 1) 0 WallClock 0
    , Rule 1974 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Nov (Day 1) 0 WallClock 0
    , Rule 1977 1977 Oct (Day 1) 0 WallClock 0
    , Rule 1978 1978 Apr (Day 30) 0 WallClock 60
    , Rule 1978 1978 Sep (Day 30) 0 WallClock 0
    , Rule 1985 1985 Apr (Day 1) 0 WallClock 60
    , Rule 1985 1985 Oct (Day 1) 0 WallClock 0
    , Rule 1986 1988 Apr (Next Fri 1) 0 WallClock 60
    , Rule 1986 1990 Oct (Next Fri 1) 0 WallClock 0
    , Rule 1989 1989 May (Day 8) 0 WallClock 60
    , Rule 1990 1990 Apr (Day 27) 0 WallClock 60
    , Rule 1991 1991 Apr (Day 17) 0 WallClock 60
    , Rule 1991 1991 Sep (Day 27) 0 WallClock 0
    , Rule 1992 1992 Apr (Day 10) 0 WallClock 60
    , Rule 1992 1993 Oct (Next Fri 1) 0 WallClock 0
    , Rule 1993 1998 Apr (Next Fri 1) 0 WallClock 60
    , Rule 1994 1994 Sep (Next Fri 15) 0 WallClock 0
    , Rule 1995 1998 Sep (Next Fri 15) 0 Standard 0
    , Rule 1999 1999 Jul (Day 1) 0 Standard 60
    , Rule 1999 2002 Sep (Last Fri) 0 Standard 0
    , Rule 2000 2001 Mar (Last Thu) 0 Standard 60
    , Rule 2002 2012 Mar (Last Thu) 1440 WallClock 60
    , Rule 2003 2003 Oct (Day 24) 0 Standard 0
    , Rule 2004 2004 Oct (Day 15) 0 Standard 0
    , Rule 2005 2005 Sep (Last Fri) 0 Standard 0
    , Rule 2006 2011 Oct (Last Fri) 0 Standard 0
    , Rule 2013 2013 Dec (Day 20) 0 WallClock 0
    , Rule 2014 2021 Mar (Last Thu) 1440 WallClock 60
    , Rule 2014 maxYear Oct (Last Fri) 0 Standard 0
    , Rule 2022 maxYear Feb (Last Thu) 1440 WallClock 60
    ]


rules_Kyrgyz : List Rule
rules_Kyrgyz =
    [ Rule 1992 1996 Apr (Next Sun 7) 0 Standard 60
    , Rule 1992 1996 Sep (Last Sun) 0 WallClock 0
    , Rule 1997 2005 Mar (Last Sun) 150 WallClock 60
    , Rule 1997 2004 Oct (Last Sun) 150 WallClock 0
    ]


rules_LH : List Rule
rules_LH =
    [ Rule 1981 1984 Oct (Last Sun) 120 WallClock 60
    , Rule 1982 1985 Mar (Next Sun 1) 120 WallClock 0
    , Rule 1985 1985 Oct (Last Sun) 120 WallClock 30
    , Rule 1986 1989 Mar (Next Sun 15) 120 WallClock 0
    , Rule 1986 1986 Oct (Day 19) 120 WallClock 30
    , Rule 1987 1999 Oct (Last Sun) 120 WallClock 30
    , Rule 1990 1995 Mar (Next Sun 1) 120 WallClock 0
    , Rule 1996 2005 Mar (Last Sun) 120 WallClock 0
    , Rule 2000 2000 Aug (Last Sun) 120 WallClock 30
    , Rule 2001 2007 Oct (Last Sun) 120 WallClock 30
    , Rule 2006 2006 Apr (Next Sun 1) 120 WallClock 0
    , Rule 2007 2007 Mar (Last Sun) 120 WallClock 0
    , Rule 2008 maxYear Apr (Next Sun 1) 120 WallClock 0
    , Rule 2008 maxYear Oct (Next Sun 1) 120 WallClock 30
    ]


rules_Latvia : List Rule
rules_Latvia =
    [ Rule 1989 1996 Mar (Last Sun) 120 Standard 60
    , Rule 1989 1996 Sep (Last Sun) 120 Standard 0
    ]


rules_Lebanon : List Rule
rules_Lebanon =
    [ Rule 1972 1972 Jun (Day 22) 0 WallClock 60
    , Rule 1972 1977 Oct (Day 1) 0 WallClock 0
    , Rule 1973 1977 May (Day 1) 0 WallClock 60
    , Rule 1978 1978 Apr (Day 30) 0 WallClock 60
    , Rule 1978 1978 Sep (Day 30) 0 WallClock 0
    , Rule 1984 1987 May (Day 1) 0 WallClock 60
    , Rule 1984 1991 Oct (Day 16) 0 WallClock 0
    , Rule 1988 1988 Jun (Day 1) 0 WallClock 60
    , Rule 1989 1989 May (Day 10) 0 WallClock 60
    , Rule 1990 1992 May (Day 1) 0 WallClock 60
    , Rule 1992 1992 Oct (Day 4) 0 WallClock 0
    , Rule 1993 maxYear Mar (Last Sun) 0 WallClock 60
    , Rule 1993 1998 Sep (Last Sun) 0 WallClock 0
    , Rule 1999 maxYear Oct (Last Sun) 0 WallClock 0
    ]


rules_Libya : List Rule
rules_Libya =
    [ Rule 1982 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1982 1985 Oct (Day 1) 0 WallClock 0
    , Rule 1985 1985 Apr (Day 6) 0 WallClock 60
    , Rule 1986 1986 Apr (Day 4) 0 WallClock 60
    , Rule 1986 1986 Oct (Day 3) 0 WallClock 0
    , Rule 1987 1989 Apr (Day 1) 0 WallClock 60
    , Rule 1987 1989 Oct (Day 1) 0 WallClock 0
    , Rule 1997 1997 Apr (Day 4) 0 WallClock 60
    , Rule 1997 1997 Oct (Day 4) 0 WallClock 0
    , Rule 2013 2013 Mar (Last Fri) 60 WallClock 60
    , Rule 2013 2013 Oct (Last Fri) 120 WallClock 0
    ]


rules_Macau : List Rule
rules_Macau =
    [ Rule 1965 1973 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1967 1976 Oct (Next Sun 16) 210 WallClock 0
    , Rule 1973 1973 Dec (Day 30) 210 WallClock 60
    , Rule 1975 1976 Apr (Next Sun 16) 210 WallClock 60
    , Rule 1979 1979 May (Day 13) 210 WallClock 60
    , Rule 1979 1979 Oct (Next Sun 16) 210 WallClock 0
    ]


rules_Malta : List Rule
rules_Malta =
    [ Rule 1973 1973 Mar (Day 31) 0 Standard 60
    , Rule 1973 1973 Sep (Day 29) 0 Standard 0
    , Rule 1974 1974 Apr (Day 21) 0 Standard 60
    , Rule 1974 1974 Sep (Day 16) 0 Standard 0
    , Rule 1975 1979 Apr (Next Sun 15) 120 WallClock 60
    , Rule 1975 1980 Sep (Next Sun 15) 120 WallClock 0
    , Rule 1980 1980 Mar (Day 31) 120 WallClock 60
    ]


rules_Mauritius : List Rule
rules_Mauritius =
    [ Rule 1982 1982 Oct (Day 10) 0 WallClock 60
    , Rule 1983 1983 Mar (Day 21) 0 WallClock 0
    , Rule 2008 2008 Oct (Last Sun) 120 WallClock 60
    , Rule 2009 2009 Mar (Last Sun) 120 WallClock 0
    ]


rules_Mexico : List Rule
rules_Mexico =
    [ Rule 1996 2000 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1996 2000 Oct (Last Sun) 120 WallClock 0
    , Rule 2001 2001 May (Next Sun 1) 120 WallClock 60
    , Rule 2001 2001 Sep (Last Sun) 120 WallClock 0
    , Rule 2002 maxYear Apr (Next Sun 1) 120 WallClock 60
    , Rule 2002 maxYear Oct (Last Sun) 120 WallClock 0
    ]


rules_Moldova : List Rule
rules_Moldova =
    [ Rule 1997 maxYear Mar (Last Sun) 120 WallClock 60
    , Rule 1997 maxYear Oct (Last Sun) 180 WallClock 0
    ]


rules_Moncton : List Rule
rules_Moncton =
    [ Rule 1946 1972 Apr (Last Sun) 120 WallClock 60
    , Rule 1957 1972 Oct (Last Sun) 120 WallClock 0
    , Rule 1993 2006 Apr (Next Sun 1) 1 WallClock 60
    , Rule 1993 2006 Oct (Last Sun) 1 WallClock 0
    ]


rules_Mongol : List Rule
rules_Mongol =
    [ Rule 1983 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1983 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1985 1998 Mar (Last Sun) 0 WallClock 60
    , Rule 1984 1998 Sep (Last Sun) 0 WallClock 0
    , Rule 2001 2001 Apr (Last Sat) 120 WallClock 60
    , Rule 2001 2006 Sep (Last Sat) 120 WallClock 0
    , Rule 2002 2006 Mar (Last Sat) 120 WallClock 60
    , Rule 2015 2016 Mar (Last Sat) 120 WallClock 60
    , Rule 2015 2016 Sep (Last Sat) 0 WallClock 0
    ]


rules_Morocco : List Rule
rules_Morocco =
    [ Rule 1974 1974 Jun (Day 24) 0 WallClock 60
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1976 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Aug (Day 1) 0 WallClock 0
    , Rule 1977 1977 Sep (Day 28) 0 WallClock 0
    , Rule 1978 1978 Jun (Day 1) 0 WallClock 60
    , Rule 1978 1978 Aug (Day 4) 0 WallClock 0
    , Rule 2008 2008 Jun (Day 1) 0 WallClock 60
    , Rule 2008 2008 Sep (Day 1) 0 WallClock 0
    , Rule 2009 2009 Jun (Day 1) 0 WallClock 60
    , Rule 2009 2009 Aug (Day 21) 0 WallClock 0
    , Rule 2010 2010 May (Day 2) 0 WallClock 60
    , Rule 2010 2010 Aug (Day 8) 0 WallClock 0
    , Rule 2011 2011 Apr (Day 3) 0 WallClock 60
    , Rule 2011 2011 Jul (Day 31) 0 WallClock 0
    , Rule 2012 2013 Apr (Last Sun) 120 WallClock 60
    , Rule 2012 2012 Jul (Day 20) 180 WallClock 0
    , Rule 2012 2012 Aug (Day 20) 120 WallClock 60
    , Rule 2012 2012 Sep (Day 30) 180 WallClock 0
    , Rule 2013 2013 Jul (Day 7) 180 WallClock 0
    , Rule 2013 2013 Aug (Day 10) 120 WallClock 60
    , Rule 2013 2018 Oct (Last Sun) 180 WallClock 0
    , Rule 2014 2018 Mar (Last Sun) 120 WallClock 60
    , Rule 2014 2014 Jun (Day 28) 180 WallClock 0
    , Rule 2014 2014 Aug (Day 2) 120 WallClock 60
    , Rule 2015 2015 Jun (Day 14) 180 WallClock 0
    , Rule 2015 2015 Jul (Day 19) 120 WallClock 60
    , Rule 2016 2016 Jun (Day 5) 180 WallClock 0
    , Rule 2016 2016 Jul (Day 10) 120 WallClock 60
    , Rule 2017 2017 May (Day 21) 180 WallClock 0
    , Rule 2017 2017 Jul (Day 2) 120 WallClock 60
    , Rule 2018 2018 May (Day 13) 180 WallClock 0
    , Rule 2018 2018 Jun (Day 17) 120 WallClock 60
    , Rule 2019 2019 May (Day 5) 180 WallClock -60
    , Rule 2019 2019 Jun (Day 9) 120 WallClock 0
    , Rule 2020 2020 Apr (Day 19) 180 WallClock -60
    , Rule 2020 2020 May (Day 31) 120 WallClock 0
    , Rule 2021 2021 Apr (Day 11) 180 WallClock -60
    , Rule 2021 2021 May (Day 16) 120 WallClock 0
    , Rule 2022 2022 Mar (Day 27) 180 WallClock -60
    , Rule 2022 2022 May (Day 8) 120 WallClock 0
    , Rule 2023 2023 Mar (Day 19) 180 WallClock -60
    , Rule 2023 2023 Apr (Day 30) 120 WallClock 0
    , Rule 2024 2024 Mar (Day 10) 180 WallClock -60
    , Rule 2024 2024 Apr (Day 14) 120 WallClock 0
    , Rule 2025 2025 Feb (Day 23) 180 WallClock -60
    , Rule 2025 2025 Apr (Day 6) 120 WallClock 0
    , Rule 2026 2026 Feb (Day 15) 180 WallClock -60
    , Rule 2026 2026 Mar (Day 22) 120 WallClock 0
    , Rule 2027 2027 Feb (Day 7) 180 WallClock -60
    , Rule 2027 2027 Mar (Day 14) 120 WallClock 0
    , Rule 2028 2028 Jan (Day 23) 180 WallClock -60
    , Rule 2028 2028 Mar (Day 5) 120 WallClock 0
    , Rule 2029 2029 Jan (Day 14) 180 WallClock -60
    , Rule 2029 2029 Feb (Day 18) 120 WallClock 0
    , Rule 2029 2029 Dec (Day 30) 180 WallClock -60
    , Rule 2030 2030 Feb (Day 10) 120 WallClock 0
    , Rule 2030 2030 Dec (Day 22) 180 WallClock -60
    , Rule 2031 2031 Feb (Day 2) 120 WallClock 0
    , Rule 2031 2031 Dec (Day 14) 180 WallClock -60
    , Rule 2032 2032 Jan (Day 18) 120 WallClock 0
    , Rule 2032 2032 Nov (Day 28) 180 WallClock -60
    , Rule 2033 2033 Jan (Day 9) 120 WallClock 0
    , Rule 2033 2033 Nov (Day 20) 180 WallClock -60
    , Rule 2033 2033 Dec (Day 25) 120 WallClock 0
    , Rule 2034 2034 Nov (Day 5) 180 WallClock -60
    , Rule 2034 2034 Dec (Day 17) 120 WallClock 0
    , Rule 2035 2035 Oct (Day 28) 180 WallClock -60
    , Rule 2035 2035 Dec (Day 9) 120 WallClock 0
    , Rule 2036 2036 Oct (Day 19) 180 WallClock -60
    , Rule 2036 2036 Nov (Day 23) 120 WallClock 0
    , Rule 2037 2037 Oct (Day 4) 180 WallClock -60
    , Rule 2037 2037 Nov (Day 15) 120 WallClock 0
    ]


rules_NC : List Rule
rules_NC =
    [ Rule 1977 1978 Dec (Next Sun 1) 0 WallClock 60
    , Rule 1978 1979 Feb (Day 27) 0 WallClock 0
    , Rule 1996 1996 Dec (Day 1) 120 Standard 60
    , Rule 1997 1997 Mar (Day 2) 120 Standard 0
    ]


rules_NT_YK : List Rule
rules_NT_YK =
    [ Rule 1980 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1980 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    ]


rules_NZ : List Rule
rules_NZ =
    [ Rule 1974 1974 Nov (Next Sun 1) 120 Standard 60
    , Rule 1975 1975 Feb (Last Sun) 120 Standard 0
    , Rule 1975 1988 Oct (Last Sun) 120 Standard 60
    , Rule 1976 1989 Mar (Next Sun 1) 120 Standard 0
    , Rule 1989 1989 Oct (Next Sun 8) 120 Standard 60
    , Rule 1990 2006 Oct (Next Sun 1) 120 Standard 60
    , Rule 1990 2007 Mar (Next Sun 15) 120 Standard 0
    , Rule 2007 maxYear Sep (Last Sun) 120 Standard 60
    , Rule 2008 maxYear Apr (Next Sun 1) 120 Standard 0
    ]


rules_Namibia : List Rule
rules_Namibia =
    [ Rule 1994 1994 Mar (Day 21) 0 WallClock -60
    , Rule 1994 2017 Sep (Next Sun 1) 120 WallClock 0
    , Rule 1995 2017 Apr (Next Sun 1) 120 WallClock -60
    ]


rules_Nic : List Rule
rules_Nic =
    [ Rule 1979 1980 Mar (Next Sun 16) 0 WallClock 60
    , Rule 1979 1980 Jun (Next Mon 23) 0 WallClock 0
    , Rule 2005 2005 Apr (Day 10) 0 WallClock 60
    , Rule 2005 2005 Oct (Next Sun 1) 0 WallClock 0
    , Rule 2006 2006 Apr (Day 30) 120 WallClock 60
    , Rule 2006 2006 Oct (Next Sun 1) 60 WallClock 0
    ]


rules_PRC : List Rule
rules_PRC =
    [ Rule 1986 1986 May (Day 4) 120 WallClock 60
    , Rule 1986 1991 Sep (Next Sun 11) 120 WallClock 0
    , Rule 1987 1991 Apr (Next Sun 11) 120 WallClock 60
    ]


rules_Pakistan : List Rule
rules_Pakistan =
    [ Rule 2002 2002 Apr (Next Sun 2) 0 WallClock 60
    , Rule 2002 2002 Oct (Next Sun 2) 0 WallClock 0
    , Rule 2008 2008 Jun (Day 1) 0 WallClock 60
    , Rule 2008 2009 Nov (Day 1) 0 WallClock 0
    , Rule 2009 2009 Apr (Day 15) 0 WallClock 60
    ]


rules_Palestine : List Rule
rules_Palestine =
    [ Rule 1999 2005 Apr (Next Fri 15) 0 WallClock 60
    , Rule 1999 2003 Oct (Next Fri 15) 0 WallClock 0
    , Rule 2004 2004 Oct (Day 1) 60 WallClock 0
    , Rule 2005 2005 Oct (Day 4) 120 WallClock 0
    , Rule 2006 2007 Apr (Day 1) 0 WallClock 60
    , Rule 2006 2006 Sep (Day 22) 0 WallClock 0
    , Rule 2007 2007 Sep (Day 13) 120 WallClock 0
    , Rule 2008 2009 Mar (Last Fri) 0 WallClock 60
    , Rule 2008 2008 Sep (Day 1) 0 WallClock 0
    , Rule 2009 2009 Sep (Day 4) 60 WallClock 0
    , Rule 2010 2010 Mar (Day 26) 0 WallClock 60
    , Rule 2010 2010 Aug (Day 11) 0 WallClock 0
    , Rule 2011 2011 Apr (Day 1) 1 WallClock 60
    , Rule 2011 2011 Aug (Day 1) 0 WallClock 0
    , Rule 2011 2011 Aug (Day 30) 0 WallClock 60
    , Rule 2011 2011 Sep (Day 30) 0 WallClock 0
    , Rule 2012 2014 Mar (Last Thu) 1440 WallClock 60
    , Rule 2012 2012 Sep (Day 21) 60 WallClock 0
    , Rule 2013 2013 Sep (Day 27) 0 WallClock 0
    , Rule 2014 2014 Oct (Day 24) 0 WallClock 0
    , Rule 2015 2015 Mar (Day 28) 0 WallClock 60
    , Rule 2015 2015 Oct (Day 23) 60 WallClock 0
    , Rule 2016 2018 Mar (Next Sat 24) 60 WallClock 60
    , Rule 2016 2018 Oct (Next Sat 24) 60 WallClock 0
    , Rule 2019 2019 Mar (Day 29) 0 WallClock 60
    , Rule 2019 2019 Oct (Next Sat 24) 0 WallClock 0
    , Rule 2020 2021 Mar (Next Sat 24) 0 WallClock 60
    , Rule 2020 2020 Oct (Day 24) 60 WallClock 0
    , Rule 2021 maxYear Oct (Next Fri 23) 60 WallClock 0
    , Rule 2022 maxYear Mar (Next Sun 25) 0 WallClock 60
    ]


rules_Para : List Rule
rules_Para =
    [ Rule 1975 1988 Oct (Day 1) 0 WallClock 60
    , Rule 1975 1978 Mar (Day 1) 0 WallClock 0
    , Rule 1979 1991 Apr (Day 1) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 22) 0 WallClock 60
    , Rule 1990 1990 Oct (Day 1) 0 WallClock 60
    , Rule 1991 1991 Oct (Day 6) 0 WallClock 60
    , Rule 1992 1992 Mar (Day 1) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 5) 0 WallClock 60
    , Rule 1993 1993 Mar (Day 31) 0 WallClock 0
    , Rule 1993 1995 Oct (Day 1) 0 WallClock 60
    , Rule 1994 1995 Feb (Last Sun) 0 WallClock 0
    , Rule 1996 1996 Mar (Day 1) 0 WallClock 0
    , Rule 1996 2001 Oct (Next Sun 1) 0 WallClock 60
    , Rule 1997 1997 Feb (Last Sun) 0 WallClock 0
    , Rule 1998 2001 Mar (Next Sun 1) 0 WallClock 0
    , Rule 2002 2004 Apr (Next Sun 1) 0 WallClock 0
    , Rule 2002 2003 Sep (Next Sun 1) 0 WallClock 60
    , Rule 2004 2009 Oct (Next Sun 15) 0 WallClock 60
    , Rule 2005 2009 Mar (Next Sun 8) 0 WallClock 0
    , Rule 2010 maxYear Oct (Next Sun 1) 0 WallClock 60
    , Rule 2010 2012 Apr (Next Sun 8) 0 WallClock 0
    , Rule 2013 maxYear Mar (Next Sun 22) 0 WallClock 0
    ]


rules_Peru : List Rule
rules_Peru =
    [ Rule 1986 1987 Jan (Day 1) 0 WallClock 60
    , Rule 1986 1987 Apr (Day 1) 0 WallClock 0
    , Rule 1990 1990 Jan (Day 1) 0 WallClock 60
    , Rule 1990 1990 Apr (Day 1) 0 WallClock 0
    , Rule 1994 1994 Jan (Day 1) 0 WallClock 60
    , Rule 1994 1994 Apr (Day 1) 0 WallClock 0
    ]


rules_Phil : List Rule
rules_Phil =
    [ Rule 1978 1978 Mar (Day 22) 0 WallClock 60
    , Rule 1978 1978 Sep (Day 21) 0 WallClock 0
    ]


rules_Port : List Rule
rules_Port =
    [ Rule 1977 1977 Mar (Day 27) 0 Standard 60
    , Rule 1977 1977 Sep (Day 25) 0 Standard 0
    , Rule 1978 1979 Apr (Next Sun 1) 0 Standard 60
    , Rule 1978 1978 Oct (Day 1) 0 Standard 0
    , Rule 1979 1982 Sep (Last Sun) 60 Standard 0
    , Rule 1980 1980 Mar (Last Sun) 0 Standard 60
    , Rule 1981 1982 Mar (Last Sun) 60 Standard 60
    , Rule 1983 1983 Mar (Last Sun) 120 Standard 60
    ]


rules_ROK : List Rule
rules_ROK =
    [ Rule 1987 1988 May (Next Sun 8) 120 WallClock 60
    , Rule 1987 1988 Oct (Next Sun 8) 180 WallClock 0
    ]


rules_Romania : List Rule
rules_Romania =
    [ Rule 1979 1979 May (Day 27) 0 WallClock 60
    , Rule 1979 1979 Sep (Last Sun) 0 WallClock 0
    , Rule 1980 1980 Apr (Day 5) 1380 WallClock 60
    , Rule 1980 1980 Sep (Last Sun) 60 WallClock 0
    , Rule 1991 1993 Mar (Last Sun) 0 Standard 60
    , Rule 1991 1993 Sep (Last Sun) 0 Standard 0
    ]


rules_Russia : List Rule
rules_Russia =
    [ Rule 1981 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1981 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1985 2010 Mar (Last Sun) 120 Standard 60
    , Rule 1996 2010 Oct (Last Sun) 120 Standard 0
    ]


rules_RussiaAsia : List Rule
rules_RussiaAsia =
    [ Rule 1981 1984 Apr (Day 1) 0 WallClock 60
    , Rule 1981 1983 Oct (Day 1) 0 WallClock 0
    , Rule 1984 1995 Sep (Last Sun) 120 Standard 0
    , Rule 1985 2010 Mar (Last Sun) 120 Standard 60
    , Rule 1996 2010 Oct (Last Sun) 120 Standard 0
    ]


rules_Salv : List Rule
rules_Salv =
    [ Rule 1987 1988 May (Next Sun 1) 0 WallClock 60
    , Rule 1987 1988 Sep (Last Sun) 0 WallClock 0
    ]


rules_SanLuis : List Rule
rules_SanLuis =
    [ Rule 2008 2009 Mar (Next Sun 8) 0 WallClock 0
    , Rule 2007 2008 Oct (Next Sun 8) 0 WallClock 60
    ]


rules_Spain : List Rule
rules_Spain =
    [ Rule 1974 1975 Apr (Next Sat 12) 1380 WallClock 60
    , Rule 1974 1975 Oct (Next Sun 1) 60 WallClock 0
    , Rule 1976 1976 Mar (Day 27) 1380 WallClock 60
    , Rule 1976 1977 Sep (Last Sun) 60 WallClock 0
    , Rule 1977 1977 Apr (Day 2) 1380 WallClock 60
    , Rule 1978 1978 Apr (Day 2) 120 Standard 60
    , Rule 1978 1978 Oct (Day 1) 120 Standard 0
    ]


rules_SpainAfrica : List Rule
rules_SpainAfrica =
    [ Rule 1974 1974 Jun (Day 24) 0 WallClock 60
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1976 1977 May (Day 1) 0 WallClock 60
    , Rule 1976 1976 Aug (Day 1) 0 WallClock 0
    , Rule 1977 1977 Sep (Day 28) 0 WallClock 0
    , Rule 1978 1978 Jun (Day 1) 0 WallClock 60
    , Rule 1978 1978 Aug (Day 4) 0 WallClock 0
    ]


rules_StJohns : List Rule
rules_StJohns =
    [ Rule 1951 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1960 1986 Oct (Last Sun) 120 WallClock 0
    , Rule 1987 1987 Apr (Next Sun 1) 1 WallClock 60
    , Rule 1987 2006 Oct (Last Sun) 1 WallClock 0
    , Rule 1988 1988 Apr (Next Sun 1) 1 WallClock 120
    , Rule 1989 2006 Apr (Next Sun 1) 1 WallClock 60
    , Rule 2007 2011 Mar (Next Sun 8) 1 WallClock 60
    , Rule 2007 2010 Nov (Next Sun 1) 1 WallClock 0
    ]


rules_Sudan : List Rule
rules_Sudan =
    [ Rule 1970 1970 May (Day 1) 0 WallClock 60
    , Rule 1970 1985 Oct (Day 15) 0 WallClock 0
    , Rule 1971 1971 Apr (Day 30) 0 WallClock 60
    , Rule 1972 1985 Apr (Last Sun) 0 WallClock 60
    ]


rules_Syria : List Rule
rules_Syria =
    [ Rule 1966 1976 Oct (Day 1) 120 WallClock 0
    , Rule 1967 1978 May (Day 1) 120 WallClock 60
    , Rule 1977 1978 Sep (Day 1) 120 WallClock 0
    , Rule 1983 1984 Apr (Day 9) 120 WallClock 60
    , Rule 1983 1984 Oct (Day 1) 120 WallClock 0
    , Rule 1986 1986 Feb (Day 16) 120 WallClock 60
    , Rule 1986 1986 Oct (Day 9) 120 WallClock 0
    , Rule 1987 1987 Mar (Day 1) 120 WallClock 60
    , Rule 1987 1988 Oct (Day 31) 120 WallClock 0
    , Rule 1988 1988 Mar (Day 15) 120 WallClock 60
    , Rule 1989 1989 Mar (Day 31) 120 WallClock 60
    , Rule 1989 1989 Oct (Day 1) 120 WallClock 0
    , Rule 1990 1990 Apr (Day 1) 120 WallClock 60
    , Rule 1990 1990 Sep (Day 30) 120 WallClock 0
    , Rule 1991 1991 Apr (Day 1) 0 WallClock 60
    , Rule 1991 1992 Oct (Day 1) 0 WallClock 0
    , Rule 1992 1992 Apr (Day 8) 0 WallClock 60
    , Rule 1993 1993 Mar (Day 26) 0 WallClock 60
    , Rule 1993 1993 Sep (Day 25) 0 WallClock 0
    , Rule 1994 1996 Apr (Day 1) 0 WallClock 60
    , Rule 1994 2005 Oct (Day 1) 0 WallClock 0
    , Rule 1997 1998 Mar (Last Mon) 0 WallClock 60
    , Rule 1999 2006 Apr (Day 1) 0 WallClock 60
    , Rule 2006 2006 Sep (Day 22) 0 WallClock 0
    , Rule 2007 2007 Mar (Last Fri) 0 WallClock 60
    , Rule 2007 2007 Nov (Next Fri 1) 0 WallClock 0
    , Rule 2008 2008 Apr (Next Fri 1) 0 WallClock 60
    , Rule 2008 2008 Nov (Day 1) 0 WallClock 0
    , Rule 2009 2009 Mar (Last Fri) 0 WallClock 60
    , Rule 2010 2011 Apr (Next Fri 1) 0 WallClock 60
    , Rule 2012 maxYear Mar (Last Fri) 0 WallClock 60
    , Rule 2009 maxYear Oct (Last Fri) 0 WallClock 0
    ]


rules_Taiwan : List Rule
rules_Taiwan =
    [ Rule 1974 1975 Apr (Day 1) 0 WallClock 60
    , Rule 1974 1975 Oct (Day 1) 0 WallClock 0
    , Rule 1979 1979 Jul (Day 1) 0 WallClock 60
    , Rule 1979 1979 Oct (Day 1) 0 WallClock 0
    ]


rules_Thule : List Rule
rules_Thule =
    [ Rule 1991 1992 Mar (Last Sun) 120 WallClock 60
    , Rule 1991 1992 Sep (Last Sun) 120 WallClock 0
    , Rule 1993 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1993 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Tonga : List Rule
rules_Tonga =
    [ Rule 1999 1999 Oct (Day 7) 120 Standard 60
    , Rule 2000 2000 Mar (Day 19) 120 Standard 0
    , Rule 2000 2001 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2001 2002 Jan (Last Sun) 120 WallClock 0
    , Rule 2016 2016 Nov (Next Sun 1) 120 WallClock 60
    , Rule 2017 2017 Jan (Next Sun 15) 180 WallClock 0
    ]


rules_Toronto : List Rule
rules_Toronto =
    [ Rule 1950 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1957 1973 Oct (Last Sun) 120 WallClock 0
    ]


rules_Troll : List Rule
rules_Troll =
    [ Rule 2005 maxYear Mar (Last Sun) 60 Universal 120
    , Rule 2004 maxYear Oct (Last Sun) 60 Universal 0
    ]


rules_Tunisia : List Rule
rules_Tunisia =
    [ Rule 1977 1977 Apr (Day 30) 0 Standard 60
    , Rule 1977 1977 Sep (Day 24) 0 Standard 0
    , Rule 1978 1978 May (Day 1) 0 Standard 60
    , Rule 1978 1978 Oct (Day 1) 0 Standard 0
    , Rule 1988 1988 Jun (Day 1) 0 Standard 60
    , Rule 1988 1990 Sep (Last Sun) 0 Standard 0
    , Rule 1989 1989 Mar (Day 26) 0 Standard 60
    , Rule 1990 1990 May (Day 1) 0 Standard 60
    , Rule 2005 2005 May (Day 1) 0 Standard 60
    , Rule 2005 2005 Sep (Day 30) 60 Standard 0
    , Rule 2006 2008 Mar (Last Sun) 120 Standard 60
    , Rule 2006 2008 Oct (Last Sun) 120 Standard 0
    ]


rules_Turkey : List Rule
rules_Turkey =
    [ Rule 1973 1973 Jun (Day 3) 60 WallClock 60
    , Rule 1973 1976 Oct (Next Sun 31) 120 WallClock 0
    , Rule 1974 1974 Mar (Day 31) 120 WallClock 60
    , Rule 1975 1975 Mar (Day 22) 120 WallClock 60
    , Rule 1976 1976 Mar (Day 21) 120 WallClock 60
    , Rule 1977 1978 Apr (Next Sun 1) 120 WallClock 60
    , Rule 1977 1978 Oct (Next Sun 15) 120 WallClock 0
    , Rule 1978 1978 Jun (Day 29) 0 WallClock 0
    , Rule 1983 1983 Jul (Day 31) 120 WallClock 60
    , Rule 1983 1983 Oct (Day 2) 120 WallClock 0
    , Rule 1985 1985 Apr (Day 20) 60 Standard 60
    , Rule 1985 1985 Sep (Day 28) 60 Standard 0
    , Rule 1986 1993 Mar (Last Sun) 60 Standard 60
    , Rule 1986 1995 Sep (Last Sun) 60 Standard 0
    , Rule 1994 1994 Mar (Day 20) 60 Standard 60
    , Rule 1995 2006 Mar (Last Sun) 60 Standard 60
    , Rule 1996 2006 Oct (Last Sun) 60 Standard 0
    ]


rules_US : List Rule
rules_US =
    [ Rule 1967 2006 Oct (Last Sun) 120 WallClock 0
    , Rule 1967 1973 Apr (Last Sun) 120 WallClock 60
    , Rule 1974 1974 Jan (Day 6) 120 WallClock 60
    , Rule 1975 1975 Feb (Last Sun) 120 WallClock 60
    , Rule 1976 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1987 2006 Apr (Next Sun 1) 120 WallClock 60
    , Rule 2007 maxYear Mar (Next Sun 8) 120 WallClock 60
    , Rule 2007 maxYear Nov (Next Sun 1) 120 WallClock 0
    ]


rules_Uruguay : List Rule
rules_Uruguay =
    [ Rule 1970 1970 Apr (Day 25) 0 WallClock 60
    , Rule 1970 1970 Jun (Day 14) 0 WallClock 0
    , Rule 1972 1972 Apr (Day 23) 0 WallClock 60
    , Rule 1972 1972 Jul (Day 16) 0 WallClock 0
    , Rule 1974 1974 Jan (Day 13) 0 WallClock 90
    , Rule 1974 1974 Mar (Day 10) 0 WallClock 30
    , Rule 1974 1974 Sep (Day 1) 0 WallClock 0
    , Rule 1974 1974 Dec (Day 22) 0 WallClock 60
    , Rule 1975 1975 Mar (Day 30) 0 WallClock 0
    , Rule 1976 1976 Dec (Day 19) 0 WallClock 60
    , Rule 1977 1977 Mar (Day 6) 0 WallClock 0
    , Rule 1977 1977 Dec (Day 4) 0 WallClock 60
    , Rule 1978 1979 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1978 1978 Dec (Day 17) 0 WallClock 60
    , Rule 1979 1979 Apr (Day 29) 0 WallClock 60
    , Rule 1980 1980 Mar (Day 16) 0 WallClock 0
    , Rule 1987 1987 Dec (Day 14) 0 WallClock 60
    , Rule 1988 1988 Feb (Day 28) 0 WallClock 0
    , Rule 1988 1988 Dec (Day 11) 0 WallClock 60
    , Rule 1989 1989 Mar (Day 5) 0 WallClock 0
    , Rule 1989 1989 Oct (Day 29) 0 WallClock 60
    , Rule 1990 1990 Feb (Day 25) 0 WallClock 0
    , Rule 1990 1991 Oct (Next Sun 21) 0 WallClock 60
    , Rule 1991 1992 Mar (Next Sun 1) 0 WallClock 0
    , Rule 1992 1992 Oct (Day 18) 0 WallClock 60
    , Rule 1993 1993 Feb (Day 28) 0 WallClock 0
    , Rule 2004 2004 Sep (Day 19) 0 WallClock 60
    , Rule 2005 2005 Mar (Day 27) 120 WallClock 0
    , Rule 2005 2005 Oct (Day 9) 120 WallClock 60
    , Rule 2006 2015 Mar (Next Sun 8) 120 WallClock 0
    , Rule 2006 2014 Oct (Next Sun 1) 120 WallClock 60
    ]


rules_Vanc : List Rule
rules_Vanc =
    [ Rule 1946 1986 Apr (Last Sun) 120 WallClock 60
    , Rule 1962 2006 Oct (Last Sun) 120 WallClock 0
    ]


rules_Vanuatu : List Rule
rules_Vanuatu =
    [ Rule 1973 1973 Dec (Day 22) 720 Universal 60
    , Rule 1974 1974 Mar (Day 30) 720 Universal 0
    , Rule 1983 1991 Sep (Next Sat 22) 1440 WallClock 60
    , Rule 1984 1991 Mar (Next Sat 22) 1440 WallClock 0
    , Rule 1992 1993 Jan (Next Sat 22) 1440 WallClock 0
    , Rule 1992 1992 Oct (Next Sat 22) 1440 WallClock 60
    ]


rules_W_Eur : List Rule
rules_W_Eur =
    [ Rule 1977 1980 Apr (Next Sun 1) 60 Standard 60
    , Rule 1977 1977 Sep (Last Sun) 60 Standard 0
    , Rule 1978 1978 Oct (Day 1) 60 Standard 0
    , Rule 1979 1995 Sep (Last Sun) 60 Standard 0
    , Rule 1981 maxYear Mar (Last Sun) 60 Standard 60
    , Rule 1996 maxYear Oct (Last Sun) 60 Standard 0
    ]


rules_WS : List Rule
rules_WS =
    [ Rule 2010 2010 Sep (Last Sun) 0 WallClock 60
    , Rule 2011 2011 Apr (Next Sat 1) 240 WallClock 0
    , Rule 2011 2011 Sep (Last Sat) 180 WallClock 60
    , Rule 2012 2021 Apr (Next Sun 1) 240 WallClock 0
    , Rule 2012 2020 Sep (Last Sun) 180 WallClock 60
    ]


rules_Winn : List Rule
rules_Winn =
    [ Rule 1966 1986 Apr (Last Sun) 120 Standard 60
    , Rule 1966 2005 Oct (Last Sun) 120 Standard 0
    , Rule 1987 2005 Apr (Next Sun 1) 120 Standard 60
    ]


rules_Zion : List Rule
rules_Zion =
    [ Rule 1974 1974 Jul (Day 6) 1440 WallClock 60
    , Rule 1974 1974 Oct (Day 12) 1440 WallClock 0
    , Rule 1975 1975 Apr (Day 19) 1440 WallClock 60
    , Rule 1975 1975 Aug (Day 30) 1440 WallClock 0
    , Rule 1980 1980 Aug (Day 2) 1440 Standard 60
    , Rule 1980 1980 Sep (Day 13) 1440 Standard 0
    , Rule 1984 1984 May (Day 5) 1440 Standard 60
    , Rule 1984 1984 Aug (Day 25) 1440 Standard 0
    , Rule 1985 1985 Apr (Day 13) 1440 WallClock 60
    , Rule 1985 1985 Aug (Day 31) 1440 WallClock 0
    , Rule 1986 1986 May (Day 17) 1440 WallClock 60
    , Rule 1986 1986 Sep (Day 6) 1440 WallClock 0
    , Rule 1987 1987 Apr (Day 14) 1440 WallClock 60
    , Rule 1987 1987 Sep (Day 12) 1440 WallClock 0
    , Rule 1988 1988 Apr (Day 9) 1440 WallClock 60
    , Rule 1988 1988 Sep (Day 3) 1440 WallClock 0
    , Rule 1989 1989 Apr (Day 29) 1440 WallClock 60
    , Rule 1989 1989 Sep (Day 2) 1440 WallClock 0
    , Rule 1990 1990 Mar (Day 24) 1440 WallClock 60
    , Rule 1990 1990 Aug (Day 25) 1440 WallClock 0
    , Rule 1991 1991 Mar (Day 23) 1440 WallClock 60
    , Rule 1991 1991 Aug (Day 31) 1440 WallClock 0
    , Rule 1992 1992 Mar (Day 28) 1440 WallClock 60
    , Rule 1992 1992 Sep (Day 5) 1440 WallClock 0
    , Rule 1993 1993 Apr (Day 2) 0 WallClock 60
    , Rule 1993 1993 Sep (Day 5) 0 WallClock 0
    , Rule 1994 1994 Apr (Day 1) 0 WallClock 60
    , Rule 1994 1994 Aug (Day 28) 0 WallClock 0
    , Rule 1995 1995 Mar (Day 31) 0 WallClock 60
    , Rule 1995 1995 Sep (Day 3) 0 WallClock 0
    , Rule 1996 1996 Mar (Day 14) 1440 WallClock 60
    , Rule 1996 1996 Sep (Day 15) 1440 WallClock 0
    , Rule 1997 1997 Mar (Day 20) 1440 WallClock 60
    , Rule 1997 1997 Sep (Day 13) 1440 WallClock 0
    , Rule 1998 1998 Mar (Day 20) 0 WallClock 60
    , Rule 1998 1998 Sep (Day 6) 0 WallClock 0
    , Rule 1999 1999 Apr (Day 2) 120 WallClock 60
    , Rule 1999 1999 Sep (Day 3) 120 WallClock 0
    , Rule 2000 2000 Apr (Day 14) 120 WallClock 60
    , Rule 2000 2000 Oct (Day 6) 60 WallClock 0
    , Rule 2001 2001 Apr (Day 9) 60 WallClock 60
    , Rule 2001 2001 Sep (Day 24) 60 WallClock 0
    , Rule 2002 2002 Mar (Day 29) 60 WallClock 60
    , Rule 2002 2002 Oct (Day 7) 60 WallClock 0
    , Rule 2003 2003 Mar (Day 28) 60 WallClock 60
    , Rule 2003 2003 Oct (Day 3) 60 WallClock 0
    , Rule 2004 2004 Apr (Day 7) 60 WallClock 60
    , Rule 2004 2004 Sep (Day 22) 60 WallClock 0
    , Rule 2005 2012 Apr (Prev Fri 1) 120 WallClock 60
    , Rule 2005 2005 Oct (Day 9) 120 WallClock 0
    , Rule 2006 2006 Oct (Day 1) 120 WallClock 0
    , Rule 2007 2007 Sep (Day 16) 120 WallClock 0
    , Rule 2008 2008 Oct (Day 5) 120 WallClock 0
    , Rule 2009 2009 Sep (Day 27) 120 WallClock 0
    , Rule 2010 2010 Sep (Day 12) 120 WallClock 0
    , Rule 2011 2011 Oct (Day 2) 120 WallClock 0
    , Rule 2012 2012 Sep (Day 23) 120 WallClock 0
    , Rule 2013 maxYear Mar (Next Fri 23) 120 WallClock 60
    , Rule 2013 maxYear Oct (Last Sun) 120 WallClock 0
    ]



-- Zones


{-| `Africa/Abidjan`
-}
africa__abidjan : () -> Time2.Zone
africa__abidjan _ =
    fromSpecification "Africa/Abidjan" <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Africa/Algiers`
-}
africa__algiers : () -> Time2.Zone
africa__algiers _ =
    fromSpecification "Africa/Algiers" <|
        Zone
            [ ( ZoneState 0 (Rules rules_Algeria), DateTime 1977 Oct 21 0 WallClock )
            , ( ZoneState 60 (Rules rules_Algeria), DateTime 1979 Oct 26 0 WallClock )
            , ( ZoneState 0 (Rules rules_Algeria), DateTime 1981 May 1 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Africa/Bissau`
-}
africa__bissau : () -> Time2.Zone
africa__bissau _ =
    fromSpecification "Africa/Bissau" <|
        Zone
            [ ( ZoneState -60 (Save 0), DateTime 1975 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Cairo`
-}
africa__cairo : () -> Time2.Zone
africa__cairo _ =
    fromSpecification "Africa/Cairo" <|
        Zone
            []
            (ZoneState 120 (Rules rules_Egypt))


{-| `Africa/Casablanca`
-}
africa__casablanca : () -> Time2.Zone
africa__casablanca _ =
    fromSpecification "Africa/Casablanca" <|
        Zone
            [ ( ZoneState 0 (Rules rules_Morocco), DateTime 1984 Mar 16 0 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            , ( ZoneState 0 (Rules rules_Morocco), DateTime 2018 Oct 28 180 WallClock )
            ]
            (ZoneState 60 (Rules rules_Morocco))


{-| `Africa/Ceuta`
-}
africa__ceuta : () -> Time2.Zone
africa__ceuta _ =
    fromSpecification "Africa/Ceuta" <|
        Zone
            [ ( ZoneState 0 (Rules rules_SpainAfrica), DateTime 1984 Mar 16 0 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Africa/El_Aaiun`
-}
africa__el_aaiun : () -> Time2.Zone
africa__el_aaiun _ =
    fromSpecification "Africa/El_Aaiun" <|
        Zone
            [ ( ZoneState -60 (Save 0), DateTime 1976 Apr 14 0 WallClock )
            , ( ZoneState 0 (Rules rules_Morocco), DateTime 2018 Oct 28 180 WallClock )
            ]
            (ZoneState 60 (Rules rules_Morocco))


{-| `Africa/Johannesburg`
-}
africa__johannesburg : () -> Time2.Zone
africa__johannesburg _ =
    fromSpecification "Africa/Johannesburg" <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Juba`
-}
africa__juba : () -> Time2.Zone
africa__juba _ =
    fromSpecification "Africa/Juba" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Sudan), DateTime 2000 Jan 15 720 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2021 Feb 1 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Khartoum`
-}
africa__khartoum : () -> Time2.Zone
africa__khartoum _ =
    fromSpecification "Africa/Khartoum" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Sudan), DateTime 2000 Jan 15 720 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2017 Nov 1 0 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Lagos`
-}
africa__lagos : () -> Time2.Zone
africa__lagos _ =
    fromSpecification "Africa/Lagos" <|
        Zone
            []
            (ZoneState 60 (Save 0))


{-| `Africa/Maputo`
-}
africa__maputo : () -> Time2.Zone
africa__maputo _ =
    fromSpecification "Africa/Maputo" <|
        Zone
            []
            (ZoneState 120 (Save 0))


{-| `Africa/Monrovia`
-}
africa__monrovia : () -> Time2.Zone
africa__monrovia _ =
    fromSpecification "Africa/Monrovia" <|
        Zone
            [ ( ZoneState -45 (Save 0), DateTime 1972 Jan 7 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Nairobi`
-}
africa__nairobi : () -> Time2.Zone
africa__nairobi _ =
    fromSpecification "Africa/Nairobi" <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Africa/Ndjamena`
-}
africa__ndjamena : () -> Time2.Zone
africa__ndjamena _ =
    fromSpecification "Africa/Ndjamena" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1979 Oct 14 0 WallClock )
            , ( ZoneState 60 (Save 60), DateTime 1980 Mar 8 0 WallClock )
            ]
            (ZoneState 60 (Save 0))


{-| `Africa/Sao_Tome`
-}
africa__sao_tome : () -> Time2.Zone
africa__sao_tome _ =
    fromSpecification "Africa/Sao_Tome" <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 2018 Jan 1 60 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 2019 Jan 1 120 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `Africa/Tripoli`
-}
africa__tripoli : () -> Time2.Zone
africa__tripoli _ =
    fromSpecification "Africa/Tripoli" <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 1990 May 4 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1996 Sep 30 0 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 1997 Oct 4 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2012 Nov 10 120 WallClock )
            , ( ZoneState 60 (Rules rules_Libya), DateTime 2013 Oct 25 120 WallClock )
            ]
            (ZoneState 120 (Save 0))


{-| `Africa/Tunis`
-}
africa__tunis : () -> Time2.Zone
africa__tunis _ =
    fromSpecification "Africa/Tunis" <|
        Zone
            []
            (ZoneState 60 (Rules rules_Tunisia))


{-| `Africa/Windhoek`
-}
africa__windhoek : () -> Time2.Zone
africa__windhoek _ =
    fromSpecification "Africa/Windhoek" <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1990 Mar 21 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Namibia))


{-| `America/Adak`
-}
america__adak : () -> Time2.Zone
america__adak _ =
    fromSpecification "America/Adak" <|
        Zone
            [ ( ZoneState -660 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -600 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -600 (Rules rules_US))


{-| `America/Anchorage`
-}
america__anchorage : () -> Time2.Zone
america__anchorage _ =
    fromSpecification "America/Anchorage" <|
        Zone
            [ ( ZoneState -600 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Araguaina`
-}
america__araguaina : () -> Time2.Zone
america__araguaina _ =
    fromSpecification "America/Araguaina" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1995 Sep 14 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2012 Oct 21 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2013 Sep 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Buenos_Aires`
-}
america__argentina__buenos_aires : () -> Time2.Zone
america__argentina__buenos_aires _ =
    fromSpecification "America/Argentina/Buenos_Aires" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Catamarca`
-}
america__argentina__catamarca : () -> Time2.Zone
america__argentina__catamarca _ =
    fromSpecification "America/Argentina/Catamarca" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Cordoba`
-}
america__argentina__cordoba : () -> Time2.Zone
america__argentina__cordoba _ =
    fromSpecification "America/Argentina/Cordoba" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Jujuy`
-}
america__argentina__jujuy : () -> Time2.Zone
america__argentina__jujuy _ =
    fromSpecification "America/Argentina/Jujuy" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Mar 4 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 28 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 17 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 6 0 WallClock )
            , ( ZoneState -180 (Save 60), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/La_Rioja`
-}
america__argentina__la_rioja : () -> Time2.Zone
america__argentina__la_rioja _ =
    fromSpecification "America/Argentina/La_Rioja" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 May 7 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Mendoza`
-}
america__argentina__mendoza : () -> Time2.Zone
america__argentina__mendoza _ =
    fromSpecification "America/Argentina/Mendoza" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Mar 4 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1992 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1992 Oct 18 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 23 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Sep 26 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Rio_Gallegos`
-}
america__argentina__rio_gallegos : () -> Time2.Zone
america__argentina__rio_gallegos _ =
    fromSpecification "America/Argentina/Rio_Gallegos" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Salta`
-}
america__argentina__salta : () -> Time2.Zone
america__argentina__salta _ =
    fromSpecification "America/Argentina/Salta" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/San_Juan`
-}
america__argentina__san_juan : () -> Time2.Zone
america__argentina__san_juan _ =
    fromSpecification "America/Argentina/San_Juan" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 May 7 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 31 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jul 25 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/San_Luis`
-}
america__argentina__san_luis : () -> Time2.Zone
america__argentina__san_luis _ =
    fromSpecification "America/Argentina/San_Luis" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState -180 (Save 60), DateTime 1990 Mar 14 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1990 Oct 15 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1991 Mar 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Jun 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 31 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jul 25 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Jan 21 0 WallClock )
            , ( ZoneState -240 (Rules rules_SanLuis), DateTime 2009 Oct 11 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Argentina/Tucuman`
-}
america__argentina__tucuman : () -> Time2.Zone
america__argentina__tucuman _ =
    fromSpecification "America/Argentina/Tucuman" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1991 Mar 3 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1991 Oct 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 Jun 1 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 13 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Arg))


{-| `America/Argentina/Ushuaia`
-}
america__argentina__ushuaia : () -> Time2.Zone
america__argentina__ushuaia _ =
    fromSpecification "America/Argentina/Ushuaia" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1999 Oct 3 0 WallClock )
            , ( ZoneState -240 (Rules rules_Arg), DateTime 2000 Mar 3 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2004 May 30 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Jun 20 0 WallClock )
            , ( ZoneState -180 (Rules rules_Arg), DateTime 2008 Oct 18 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Asuncion`
-}
america__asuncion : () -> Time2.Zone
america__asuncion _ =
    fromSpecification "America/Asuncion" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1972 Oct 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1974 Apr 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Para))


{-| `America/Bahia`
-}
america__bahia : () -> Time2.Zone
america__bahia _ =
    fromSpecification "America/Bahia" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2011 Oct 16 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2012 Oct 21 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Bahia_Banderas`
-}
america__bahia_banderas : () -> Time2.Zone
america__bahia_banderas _ =
    fromSpecification "America/Bahia_Banderas" <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2010 Apr 4 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Barbados`
-}
america__barbados : () -> Time2.Zone
america__barbados _ =
    fromSpecification "America/Barbados" <|
        Zone
            []
            (ZoneState -240 (Rules rules_Barb))


{-| `America/Belem`
-}
america__belem : () -> Time2.Zone
america__belem _ =
    fromSpecification "America/Belem" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Belize`
-}
america__belize : () -> Time2.Zone
america__belize _ =
    fromSpecification "America/Belize" <|
        Zone
            []
            (ZoneState -360 (Rules rules_Belize))


{-| `America/Boa_Vista`
-}
america__boa_vista : () -> Time2.Zone
america__boa_vista _ =
    fromSpecification "America/Boa_Vista" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -240 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Bogota`
-}
america__bogota : () -> Time2.Zone
america__bogota _ =
    fromSpecification "America/Bogota" <|
        Zone
            []
            (ZoneState -300 (Rules rules_CO))


{-| `America/Boise`
-}
america__boise : () -> Time2.Zone
america__boise _ =
    fromSpecification "America/Boise" <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -420 (Save 0), DateTime 1974 Feb 3 120 WallClock )
            ]
            (ZoneState -420 (Rules rules_US))


{-| `America/Cambridge_Bay`
-}
america__cambridge_bay : () -> Time2.Zone
america__cambridge_bay _ =
    fromSpecification "America/Cambridge_Bay" <|
        Zone
            [ ( ZoneState -420 (Rules rules_NT_YK), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2000 Nov 5 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Campo_Grande`
-}
america__campo_grande : () -> Time2.Zone
america__campo_grande _ =
    fromSpecification "America/Campo_Grande" <|
        Zone
            []
            (ZoneState -240 (Rules rules_Brazil))


{-| `America/Cancun`
-}
america__cancun : () -> Time2.Zone
america__cancun _ =
    fromSpecification "America/Cancun" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1981 Dec 23 0 WallClock )
            , ( ZoneState -300 (Rules rules_Mexico), DateTime 1998 Aug 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 2015 Feb 1 120 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Caracas`
-}
america__caracas : () -> Time2.Zone
america__caracas _ =
    fromSpecification "America/Caracas" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 2007 Dec 9 180 WallClock )
            , ( ZoneState -270 (Save 0), DateTime 2016 May 1 150 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Cayenne`
-}
america__cayenne : () -> Time2.Zone
america__cayenne _ =
    fromSpecification "America/Cayenne" <|
        Zone
            []
            (ZoneState -180 (Save 0))


{-| `America/Chicago`
-}
america__chicago : () -> Time2.Zone
america__chicago _ =
    fromSpecification "America/Chicago" <|
        Zone
            []
            (ZoneState -360 (Rules rules_US))


{-| `America/Chihuahua`
-}
america__chihuahua : () -> Time2.Zone
america__chihuahua _ =
    fromSpecification "America/Chihuahua" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1998 Apr 5 180 WallClock )
            ]
            (ZoneState -420 (Rules rules_Mexico))


{-| `America/Costa_Rica`
-}
america__costa_rica : () -> Time2.Zone
america__costa_rica _ =
    fromSpecification "America/Costa_Rica" <|
        Zone
            []
            (ZoneState -360 (Rules rules_CR))


{-| `America/Cuiaba`
-}
america__cuiaba : () -> Time2.Zone
america__cuiaba _ =
    fromSpecification "America/Cuiaba" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 2003 Sep 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2004 Oct 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Brazil))


{-| `America/Danmarkshavn`
-}
america__danmarkshavn : () -> Time2.Zone
america__danmarkshavn _ =
    fromSpecification "America/Danmarkshavn" <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            , ( ZoneState -180 (Rules rules_EU), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Save 0))


{-| `America/Dawson`
-}
america__dawson : () -> Time2.Zone
america__dawson _ =
    fromSpecification "America/Dawson" <|
        Zone
            [ ( ZoneState -540 (Rules rules_NT_YK), DateTime 1973 Oct 28 0 WallClock )
            , ( ZoneState -480 (Rules rules_NT_YK), DateTime 1980 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2020 Nov 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Dawson_Creek`
-}
america__dawson_creek : () -> Time2.Zone
america__dawson_creek _ =
    fromSpecification "America/Dawson_Creek" <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1972 Aug 30 120 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Denver`
-}
america__denver : () -> Time2.Zone
america__denver _ =
    fromSpecification "America/Denver" <|
        Zone
            []
            (ZoneState -420 (Rules rules_US))


{-| `America/Detroit`
-}
america__detroit : () -> Time2.Zone
america__detroit _ =
    fromSpecification "America/Detroit" <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1975 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1975 Apr 27 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Edmonton`
-}
america__edmonton : () -> Time2.Zone
america__edmonton _ =
    fromSpecification "America/Edmonton" <|
        Zone
            [ ( ZoneState -420 (Rules rules_Edm), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Eirunepe`
-}
america__eirunepe : () -> Time2.Zone
america__eirunepe _ =
    fromSpecification "America/Eirunepe" <|
        Zone
            [ ( ZoneState -300 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1993 Sep 28 0 WallClock )
            , ( ZoneState -300 (Rules rules_Brazil), DateTime 1994 Sep 22 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2013 Nov 10 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/El_Salvador`
-}
america__el_salvador : () -> Time2.Zone
america__el_salvador _ =
    fromSpecification "America/El_Salvador" <|
        Zone
            []
            (ZoneState -360 (Rules rules_Salv))


{-| `America/Fort_Nelson`
-}
america__fort_nelson : () -> Time2.Zone
america__fort_nelson _ =
    fromSpecification "America/Fort_Nelson" <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1987 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2015 Mar 8 120 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Fortaleza`
-}
america__fortaleza : () -> Time2.Zone
america__fortaleza _ =
    fromSpecification "America/Fortaleza" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 22 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Glace_Bay`
-}
america__glace_bay : () -> Time2.Zone
america__glace_bay _ =
    fromSpecification "America/Glace_Bay" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1972 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Halifax), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Goose_Bay`
-}
america__goose_bay : () -> Time2.Zone
america__goose_bay _ =
    fromSpecification "America/Goose_Bay" <|
        Zone
            [ ( ZoneState -240 (Rules rules_StJohns), DateTime 2011 Nov 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Grand_Turk`
-}
america__grand_turk : () -> Time2.Zone
america__grand_turk _ =
    fromSpecification "America/Grand_Turk" <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1979 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 2015 Mar 8 120 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2018 Mar 11 180 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Guatemala`
-}
america__guatemala : () -> Time2.Zone
america__guatemala _ =
    fromSpecification "America/Guatemala" <|
        Zone
            []
            (ZoneState -360 (Rules rules_Guat))


{-| `America/Guayaquil`
-}
america__guayaquil : () -> Time2.Zone
america__guayaquil _ =
    fromSpecification "America/Guayaquil" <|
        Zone
            []
            (ZoneState -300 (Rules rules_Ecuador))


{-| `America/Guyana`
-}
america__guyana : () -> Time2.Zone
america__guyana _ =
    fromSpecification "America/Guyana" <|
        Zone
            [ ( ZoneState -225 (Save 0), DateTime 1975 Aug 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1992 Mar 29 60 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Halifax`
-}
america__halifax : () -> Time2.Zone
america__halifax _ =
    fromSpecification "America/Halifax" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Halifax), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Havana`
-}
america__havana : () -> Time2.Zone
america__havana _ =
    fromSpecification "America/Havana" <|
        Zone
            []
            (ZoneState -300 (Rules rules_Cuba))


{-| `America/Hermosillo`
-}
america__hermosillo : () -> Time2.Zone
america__hermosillo _ =
    fromSpecification "America/Hermosillo" <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Indiana/Indianapolis`
-}
america__indiana__indianapolis : () -> Time2.Zone
america__indiana__indianapolis _ =
    fromSpecification "America/Indiana/Indianapolis" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Knox`
-}
america__indiana__knox : () -> Time2.Zone
america__indiana__knox _ =
    fromSpecification "America/Indiana/Knox" <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 1991 Oct 27 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Indiana/Marengo`
-}
america__indiana__marengo : () -> Time2.Zone
america__indiana__marengo _ =
    fromSpecification "America/Indiana/Marengo" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1974 Jan 6 120 WallClock )
            , ( ZoneState -360 (Save 60), DateTime 1974 Oct 27 120 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1976 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Petersburg`
-}
america__indiana__petersburg : () -> Time2.Zone
america__indiana__petersburg _ =
    fromSpecification "America/Indiana/Petersburg" <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 1977 Oct 30 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Nov 4 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Tell_City`
-}
america__indiana__tell_city : () -> Time2.Zone
america__indiana__tell_city _ =
    fromSpecification "America/Indiana/Tell_City" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Indiana/Vevay`
-}
america__indiana__vevay : () -> Time2.Zone
america__indiana__vevay _ =
    fromSpecification "America/Indiana/Vevay" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Vincennes`
-}
america__indiana__vincennes : () -> Time2.Zone
america__indiana__vincennes _ =
    fromSpecification "America/Indiana/Vincennes" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Nov 4 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Indiana/Winamac`
-}
america__indiana__winamac : () -> Time2.Zone
america__indiana__winamac _ =
    fromSpecification "America/Indiana/Winamac" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2006 Apr 2 120 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 2007 Mar 11 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Inuvik`
-}
america__inuvik : () -> Time2.Zone
america__inuvik _ =
    fromSpecification "America/Inuvik" <|
        Zone
            [ ( ZoneState -480 (Rules rules_NT_YK), DateTime 1979 Apr 29 120 WallClock )
            , ( ZoneState -420 (Rules rules_NT_YK), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `America/Iqaluit`
-}
america__iqaluit : () -> Time2.Zone
america__iqaluit _ =
    fromSpecification "America/Iqaluit" <|
        Zone
            [ ( ZoneState -300 (Rules rules_NT_YK), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Jamaica`
-}
america__jamaica : () -> Time2.Zone
america__jamaica _ =
    fromSpecification "America/Jamaica" <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 1984 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Juneau`
-}
america__juneau : () -> Time2.Zone
america__juneau _ =
    fromSpecification "America/Juneau" <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1980 Apr 27 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1980 Oct 26 120 WallClock )
            , ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Kentucky/Louisville`
-}
america__kentucky__louisville : () -> Time2.Zone
america__kentucky__louisville _ =
    fromSpecification "America/Kentucky/Louisville" <|
        Zone
            [ ( ZoneState -300 (Rules rules_US), DateTime 1974 Jan 6 120 WallClock )
            , ( ZoneState -360 (Save 60), DateTime 1974 Oct 27 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/Kentucky/Monticello`
-}
america__kentucky__monticello : () -> Time2.Zone
america__kentucky__monticello _ =
    fromSpecification "America/Kentucky/Monticello" <|
        Zone
            [ ( ZoneState -360 (Rules rules_US), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_US))


{-| `America/La_Paz`
-}
america__la_paz : () -> Time2.Zone
america__la_paz _ =
    fromSpecification "America/La_Paz" <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Lima`
-}
america__lima : () -> Time2.Zone
america__lima _ =
    fromSpecification "America/Lima" <|
        Zone
            []
            (ZoneState -300 (Rules rules_Peru))


{-| `America/Los_Angeles`
-}
america__los_angeles : () -> Time2.Zone
america__los_angeles _ =
    fromSpecification "America/Los_Angeles" <|
        Zone
            []
            (ZoneState -480 (Rules rules_US))


{-| `America/Maceio`
-}
america__maceio : () -> Time2.Zone
america__maceio _ =
    fromSpecification "America/Maceio" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1995 Oct 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 1996 Sep 4 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 22 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Managua`
-}
america__managua : () -> Time2.Zone
america__managua _ =
    fromSpecification "America/Managua" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1973 May 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1975 Feb 16 0 WallClock )
            , ( ZoneState -360 (Rules rules_Nic), DateTime 1992 Jan 1 240 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1992 Sep 24 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1993 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Nic))


{-| `America/Manaus`
-}
america__manaus : () -> Time2.Zone
america__manaus _ =
    fromSpecification "America/Manaus" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 1993 Sep 28 0 WallClock )
            , ( ZoneState -240 (Rules rules_Brazil), DateTime 1994 Sep 22 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Martinique`
-}
america__martinique : () -> Time2.Zone
america__martinique _ =
    fromSpecification "America/Martinique" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1980 Apr 6 0 WallClock )
            , ( ZoneState -240 (Save 60), DateTime 1980 Sep 28 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Matamoros`
-}
america__matamoros : () -> Time2.Zone
america__matamoros _ =
    fromSpecification "America/Matamoros" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 1989 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Mazatlan`
-}
america__mazatlan : () -> Time2.Zone
america__mazatlan _ =
    fromSpecification "America/Mazatlan" <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1970 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Mexico))


{-| `America/Menominee`
-}
america__menominee : () -> Time2.Zone
america__menominee _ =
    fromSpecification "America/Menominee" <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1973 Apr 29 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Merida`
-}
america__merida : () -> Time2.Zone
america__merida _ =
    fromSpecification "America/Merida" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1981 Dec 23 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1982 Dec 2 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Metlakatla`
-}
america__metlakatla : () -> Time2.Zone
america__metlakatla _ =
    fromSpecification "America/Metlakatla" <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -480 (Save 0), DateTime 2015 Nov 1 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 2018 Nov 4 120 WallClock )
            , ( ZoneState -480 (Save 0), DateTime 2019 Jan 20 120 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Mexico_City`
-}
america__mexico_city : () -> Time2.Zone
america__mexico_city _ =
    fromSpecification "America/Mexico_City" <|
        Zone
            [ ( ZoneState -360 (Rules rules_Mexico), DateTime 2001 Sep 30 120 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 2002 Feb 20 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Miquelon`
-}
america__miquelon : () -> Time2.Zone
america__miquelon _ =
    fromSpecification "America/Miquelon" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1980 May 1 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Canada))


{-| `America/Moncton`
-}
america__moncton : () -> Time2.Zone
america__moncton _ =
    fromSpecification "America/Moncton" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Moncton), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Canada), DateTime 1993 Jan 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Moncton), DateTime 2007 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_Canada))


{-| `America/Monterrey`
-}
america__monterrey : () -> Time2.Zone
america__monterrey _ =
    fromSpecification "America/Monterrey" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_US), DateTime 1989 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Mexico))


{-| `America/Montevideo`
-}
america__montevideo : () -> Time2.Zone
america__montevideo _ =
    fromSpecification "America/Montevideo" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Uruguay), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Jan 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Mar 10 0 WallClock )
            , ( ZoneState -180 (Rules rules_Uruguay), DateTime 1974 Dec 22 0 WallClock )
            ]
            (ZoneState -180 (Rules rules_Uruguay))


{-| `America/New_York`
-}
america__new_york : () -> Time2.Zone
america__new_york _ =
    fromSpecification "America/New_York" <|
        Zone
            []
            (ZoneState -300 (Rules rules_US))


{-| `America/Nipigon`
-}
america__nipigon : () -> Time2.Zone
america__nipigon _ =
    fromSpecification "America/Nipigon" <|
        Zone
            []
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Nome`
-}
america__nome : () -> Time2.Zone
america__nome _ =
    fromSpecification "America/Nome" <|
        Zone
            [ ( ZoneState -660 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Noronha`
-}
america__noronha : () -> Time2.Zone
america__noronha _ =
    fromSpecification "America/Noronha" <|
        Zone
            [ ( ZoneState -120 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -120 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -120 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            , ( ZoneState -120 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -120 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -120 (Save 0))


{-| `America/North_Dakota/Beulah`
-}
america__north_dakota__beulah : () -> Time2.Zone
america__north_dakota__beulah _ =
    fromSpecification "America/North_Dakota/Beulah" <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 2010 Nov 7 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/North_Dakota/Center`
-}
america__north_dakota__center : () -> Time2.Zone
america__north_dakota__center _ =
    fromSpecification "America/North_Dakota/Center" <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 1992 Oct 25 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/North_Dakota/New_Salem`
-}
america__north_dakota__new_salem : () -> Time2.Zone
america__north_dakota__new_salem _ =
    fromSpecification "America/North_Dakota/New_Salem" <|
        Zone
            [ ( ZoneState -420 (Rules rules_US), DateTime 2003 Oct 26 120 WallClock )
            ]
            (ZoneState -360 (Rules rules_US))


{-| `America/Nuuk`
-}
america__nuuk : () -> Time2.Zone
america__nuuk _ =
    fromSpecification "America/Nuuk" <|
        Zone
            [ ( ZoneState -180 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            ]
            (ZoneState -180 (Rules rules_EU))


{-| `America/Ojinaga`
-}
america__ojinaga : () -> Time2.Zone
america__ojinaga _ =
    fromSpecification "America/Ojinaga" <|
        Zone
            [ ( ZoneState -360 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -360 (Rules rules_Mexico), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState -360 (Save 0), DateTime 1998 Apr 5 180 WallClock )
            , ( ZoneState -420 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_US))


{-| `America/Panama`
-}
america__panama : () -> Time2.Zone
america__panama _ =
    fromSpecification "America/Panama" <|
        Zone
            []
            (ZoneState -300 (Save 0))


{-| `America/Pangnirtung`
-}
america__pangnirtung : () -> Time2.Zone
america__pangnirtung _ =
    fromSpecification "America/Pangnirtung" <|
        Zone
            [ ( ZoneState -240 (Rules rules_NT_YK), DateTime 1995 Apr 2 120 WallClock )
            , ( ZoneState -300 (Rules rules_Canada), DateTime 1999 Oct 31 120 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2000 Oct 29 120 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Paramaribo`
-}
america__paramaribo : () -> Time2.Zone
america__paramaribo _ =
    fromSpecification "America/Paramaribo" <|
        Zone
            [ ( ZoneState -210 (Save 0), DateTime 1984 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Phoenix`
-}
america__phoenix : () -> Time2.Zone
america__phoenix _ =
    fromSpecification "America/Phoenix" <|
        Zone
            []
            (ZoneState -420 (Save 0))


{-| `America/Port-au-Prince`
-}
america__port_au_prince : () -> Time2.Zone
america__port_au_prince _ =
    fromSpecification "America/Port-au-Prince" <|
        Zone
            []
            (ZoneState -300 (Rules rules_Haiti))


{-| `America/Porto_Velho`
-}
america__porto_velho : () -> Time2.Zone
america__porto_velho _ =
    fromSpecification "America/Porto_Velho" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Puerto_Rico`
-}
america__puerto_rico : () -> Time2.Zone
america__puerto_rico _ =
    fromSpecification "America/Puerto_Rico" <|
        Zone
            []
            (ZoneState -240 (Save 0))


{-| `America/Punta_Arenas`
-}
america__punta_arenas : () -> Time2.Zone
america__punta_arenas _ =
    fromSpecification "America/Punta_Arenas" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Chile), DateTime 2016 Dec 4 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Rainy_River`
-}
america__rainy_river : () -> Time2.Zone
america__rainy_river _ =
    fromSpecification "America/Rainy_River" <|
        Zone
            []
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Rankin_Inlet`
-}
america__rankin_inlet : () -> Time2.Zone
america__rankin_inlet _ =
    fromSpecification "America/Rankin_Inlet" <|
        Zone
            [ ( ZoneState -360 (Rules rules_NT_YK), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Recife`
-}
america__recife : () -> Time2.Zone
america__recife _ =
    fromSpecification "America/Recife" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Brazil), DateTime 1990 Sep 17 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 1999 Sep 30 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2000 Oct 15 0 WallClock )
            , ( ZoneState -180 (Save 0), DateTime 2001 Sep 13 0 WallClock )
            , ( ZoneState -180 (Rules rules_Brazil), DateTime 2002 Oct 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Regina`
-}
america__regina : () -> Time2.Zone
america__regina _ =
    fromSpecification "America/Regina" <|
        Zone
            []
            (ZoneState -360 (Save 0))


{-| `America/Resolute`
-}
america__resolute : () -> Time2.Zone
america__resolute _ =
    fromSpecification "America/Resolute" <|
        Zone
            [ ( ZoneState -360 (Rules rules_NT_YK), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2001 Apr 1 180 WallClock )
            , ( ZoneState -360 (Rules rules_Canada), DateTime 2006 Oct 29 120 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2007 Mar 11 180 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Rio_Branco`
-}
america__rio_branco : () -> Time2.Zone
america__rio_branco _ =
    fromSpecification "America/Rio_Branco" <|
        Zone
            [ ( ZoneState -300 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2013 Nov 10 0 WallClock )
            ]
            (ZoneState -300 (Save 0))


{-| `America/Santarem`
-}
america__santarem : () -> Time2.Zone
america__santarem _ =
    fromSpecification "America/Santarem" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Brazil), DateTime 1988 Sep 12 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2008 Jun 24 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `America/Santiago`
-}
america__santiago : () -> Time2.Zone
america__santiago _ =
    fromSpecification "America/Santiago" <|
        Zone
            []
            (ZoneState -240 (Rules rules_Chile))


{-| `America/Santo_Domingo`
-}
america__santo_domingo : () -> Time2.Zone
america__santo_domingo _ =
    fromSpecification "America/Santo_Domingo" <|
        Zone
            [ ( ZoneState -300 (Rules rules_DR), DateTime 1974 Oct 27 0 WallClock )
            , ( ZoneState -240 (Save 0), DateTime 2000 Oct 29 120 WallClock )
            , ( ZoneState -300 (Rules rules_US), DateTime 2000 Dec 3 60 WallClock )
            ]
            (ZoneState -240 (Save 0))


{-| `America/Sao_Paulo`
-}
america__sao_paulo : () -> Time2.Zone
america__sao_paulo _ =
    fromSpecification "America/Sao_Paulo" <|
        Zone
            []
            (ZoneState -180 (Rules rules_Brazil))


{-| `America/Scoresbysund`
-}
america__scoresbysund : () -> Time2.Zone
america__scoresbysund _ =
    fromSpecification "America/Scoresbysund" <|
        Zone
            [ ( ZoneState -120 (Save 0), DateTime 1980 Apr 6 120 WallClock )
            , ( ZoneState -120 (Rules rules_C_Eur), DateTime 1981 Mar 29 0 WallClock )
            ]
            (ZoneState -60 (Rules rules_EU))


{-| `America/Sitka`
-}
america__sitka : () -> Time2.Zone
america__sitka _ =
    fromSpecification "America/Sitka" <|
        Zone
            [ ( ZoneState -480 (Rules rules_US), DateTime 1983 Oct 30 120 WallClock )
            , ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/St_Johns`
-}
america__st_johns : () -> Time2.Zone
america__st_johns _ =
    fromSpecification "America/St_Johns" <|
        Zone
            [ ( ZoneState -210 (Rules rules_StJohns), DateTime 2011 Nov 1 0 WallClock )
            ]
            (ZoneState -210 (Rules rules_Canada))


{-| `America/Swift_Current`
-}
america__swift_current : () -> Time2.Zone
america__swift_current _ =
    fromSpecification "America/Swift_Current" <|
        Zone
            [ ( ZoneState -420 (Save 0), DateTime 1972 Apr 30 120 WallClock )
            ]
            (ZoneState -360 (Save 0))


{-| `America/Tegucigalpa`
-}
america__tegucigalpa : () -> Time2.Zone
america__tegucigalpa _ =
    fromSpecification "America/Tegucigalpa" <|
        Zone
            []
            (ZoneState -360 (Rules rules_Hond))


{-| `America/Thule`
-}
america__thule : () -> Time2.Zone
america__thule _ =
    fromSpecification "America/Thule" <|
        Zone
            []
            (ZoneState -240 (Rules rules_Thule))


{-| `America/Thunder_Bay`
-}
america__thunder_bay : () -> Time2.Zone
america__thunder_bay _ =
    fromSpecification "America/Thunder_Bay" <|
        Zone
            [ ( ZoneState -300 (Rules rules_Canada), DateTime 1970 Jan 1 0 WallClock )
            , ( ZoneState -300 (Rules rules_Toronto), DateTime 1973 Jan 1 0 WallClock )
            , ( ZoneState -300 (Save 0), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Tijuana`
-}
america__tijuana : () -> Time2.Zone
america__tijuana _ =
    fromSpecification "America/Tijuana" <|
        Zone
            [ ( ZoneState -480 (Save 0), DateTime 1976 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_US), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Mexico), DateTime 2001 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_US), DateTime 2002 Feb 20 0 WallClock )
            , ( ZoneState -480 (Rules rules_Mexico), DateTime 2010 Jan 1 0 WallClock )
            ]
            (ZoneState -480 (Rules rules_US))


{-| `America/Toronto`
-}
america__toronto : () -> Time2.Zone
america__toronto _ =
    fromSpecification "America/Toronto" <|
        Zone
            [ ( ZoneState -300 (Rules rules_Toronto), DateTime 1974 Jan 1 0 WallClock )
            ]
            (ZoneState -300 (Rules rules_Canada))


{-| `America/Vancouver`
-}
america__vancouver : () -> Time2.Zone
america__vancouver _ =
    fromSpecification "America/Vancouver" <|
        Zone
            [ ( ZoneState -480 (Rules rules_Vanc), DateTime 1987 Jan 1 0 WallClock )
            ]
            (ZoneState -480 (Rules rules_Canada))


{-| `America/Whitehorse`
-}
america__whitehorse : () -> Time2.Zone
america__whitehorse _ =
    fromSpecification "America/Whitehorse" <|
        Zone
            [ ( ZoneState -480 (Rules rules_NT_YK), DateTime 1980 Jan 1 0 WallClock )
            , ( ZoneState -480 (Rules rules_Canada), DateTime 2020 Nov 1 0 WallClock )
            ]
            (ZoneState -420 (Save 0))


{-| `America/Winnipeg`
-}
america__winnipeg : () -> Time2.Zone
america__winnipeg _ =
    fromSpecification "America/Winnipeg" <|
        Zone
            [ ( ZoneState -360 (Rules rules_Winn), DateTime 2006 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Canada))


{-| `America/Yakutat`
-}
america__yakutat : () -> Time2.Zone
america__yakutat _ =
    fromSpecification "America/Yakutat" <|
        Zone
            [ ( ZoneState -540 (Rules rules_US), DateTime 1983 Nov 30 0 WallClock )
            ]
            (ZoneState -540 (Rules rules_US))


{-| `America/Yellowknife`
-}
america__yellowknife : () -> Time2.Zone
america__yellowknife _ =
    fromSpecification "America/Yellowknife" <|
        Zone
            [ ( ZoneState -420 (Rules rules_NT_YK), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState -420 (Rules rules_Canada))


{-| `Antarctica/Casey`
-}
antarctica__casey : () -> Time2.Zone
antarctica__casey _ =
    fromSpecification "Antarctica/Casey" <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2010 Mar 5 120 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2011 Oct 28 120 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2012 Feb 21 1020 Universal )
            , ( ZoneState 480 (Save 0), DateTime 2016 Oct 22 0 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2018 Mar 11 240 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2018 Oct 7 240 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2019 Mar 17 180 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2019 Oct 4 180 WallClock )
            , ( ZoneState 660 (Save 0), DateTime 2020 Mar 8 180 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2020 Oct 4 1 WallClock )
            ]
            (ZoneState 660 (Save 0))


{-| `Antarctica/Davis`
-}
antarctica__davis : () -> Time2.Zone
antarctica__davis _ =
    fromSpecification "Antarctica/Davis" <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            , ( ZoneState 300 (Save 0), DateTime 2010 Mar 10 1200 Universal )
            , ( ZoneState 420 (Save 0), DateTime 2011 Oct 28 120 WallClock )
            , ( ZoneState 300 (Save 0), DateTime 2012 Feb 21 1200 Universal )
            ]
            (ZoneState 420 (Save 0))


{-| `Antarctica/Macquarie`
-}
antarctica__macquarie : () -> Time2.Zone
antarctica__macquarie _ =
    fromSpecification "Antarctica/Macquarie" <|
        Zone
            [ ( ZoneState 600 (Rules rules_AT), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState 600 (Save 60), DateTime 2011 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AT))


{-| `Antarctica/Mawson`
-}
antarctica__mawson : () -> Time2.Zone
antarctica__mawson _ =
    fromSpecification "Antarctica/Mawson" <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 2009 Oct 18 120 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Antarctica/Palmer`
-}
antarctica__palmer : () -> Time2.Zone
antarctica__palmer _ =
    fromSpecification "Antarctica/Palmer" <|
        Zone
            [ ( ZoneState -180 (Rules rules_Arg), DateTime 1982 May 1 0 WallClock )
            , ( ZoneState -240 (Rules rules_Chile), DateTime 2016 Dec 4 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Antarctica/Rothera`
-}
antarctica__rothera : () -> Time2.Zone
antarctica__rothera _ =
    fromSpecification "Antarctica/Rothera" <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1976 Dec 1 0 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Antarctica/Troll`
-}
antarctica__troll : () -> Time2.Zone
antarctica__troll _ =
    fromSpecification "Antarctica/Troll" <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 2005 Feb 12 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_Troll))


{-| `Antarctica/Vostok`
-}
antarctica__vostok : () -> Time2.Zone
antarctica__vostok _ =
    fromSpecification "Antarctica/Vostok" <|
        Zone
            []
            (ZoneState 360 (Save 0))


{-| `Asia/Almaty`
-}
asia__almaty : () -> Time2.Zone
asia__almaty _ =
    fromSpecification "Asia/Almaty" <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Amman`
-}
asia__amman : () -> Time2.Zone
asia__amman _ =
    fromSpecification "Asia/Amman" <|
        Zone
            []
            (ZoneState 120 (Rules rules_Jordan))


{-| `Asia/Anadyr`
-}
asia__anadyr : () -> Time2.Zone
asia__anadyr _ =
    fromSpecification "Asia/Anadyr" <|
        Zone
            [ ( ZoneState 780 (Rules rules_Russia), DateTime 1982 Apr 1 0 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 720 (Save 0))


{-| `Asia/Aqtau`
-}
asia__aqtau : () -> Time2.Zone
asia__aqtau _ =
    fromSpecification "Asia/Aqtau" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1994 Sep 25 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Aqtobe`
-}
asia__aqtobe : () -> Time2.Zone
asia__aqtobe _ =
    fromSpecification "Asia/Aqtobe" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Ashgabat`
-}
asia__ashgabat : () -> Time2.Zone
asia__ashgabat _ =
    fromSpecification "Asia/Ashgabat" <|
        Zone
            [ ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Atyrau`
-}
asia__atyrau : () -> Time2.Zone
asia__atyrau _ =
    fromSpecification "Asia/Atyrau" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1999 Mar 28 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Baghdad`
-}
asia__baghdad : () -> Time2.Zone
asia__baghdad _ =
    fromSpecification "Asia/Baghdad" <|
        Zone
            [ ( ZoneState 180 (Save 0), DateTime 1982 May 1 0 WallClock )
            ]
            (ZoneState 180 (Rules rules_Iraq))


{-| `Asia/Baku`
-}
asia__baku : () -> Time2.Zone
asia__baku _ =
    fromSpecification "Asia/Baku" <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1992 Sep 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 240 (Rules rules_EUAsia), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 240 (Rules rules_Azer))


{-| `Asia/Bangkok`
-}
asia__bangkok : () -> Time2.Zone
asia__bangkok _ =
    fromSpecification "Asia/Bangkok" <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Barnaul`
-}
asia__barnaul : () -> Time2.Zone
asia__barnaul _ =
    fromSpecification "Asia/Barnaul" <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1995 May 28 0 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Beirut`
-}
asia__beirut : () -> Time2.Zone
asia__beirut _ =
    fromSpecification "Asia/Beirut" <|
        Zone
            []
            (ZoneState 120 (Rules rules_Lebanon))


{-| `Asia/Bishkek`
-}
asia__bishkek : () -> Time2.Zone
asia__bishkek _ =
    fromSpecification "Asia/Bishkek" <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Aug 31 120 WallClock )
            , ( ZoneState 300 (Rules rules_Kyrgyz), DateTime 2005 Aug 12 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Brunei`
-}
asia__brunei : () -> Time2.Zone
asia__brunei _ =
    fromSpecification "Asia/Brunei" <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Chita`
-}
asia__chita : () -> Time2.Zone
asia__chita _ =
    fromSpecification "Asia/Chita" <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 480 (Save 0), DateTime 2016 Mar 27 120 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Choibalsan`
-}
asia__choibalsan : () -> Time2.Zone
asia__choibalsan _ =
    fromSpecification "Asia/Choibalsan" <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 1978 Jan 1 0 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 1983 Apr 1 0 WallClock )
            , ( ZoneState 540 (Rules rules_Mongol), DateTime 2008 Mar 31 0 WallClock )
            ]
            (ZoneState 480 (Rules rules_Mongol))


{-| `Asia/Colombo`
-}
asia__colombo : () -> Time2.Zone
asia__colombo _ =
    fromSpecification "Asia/Colombo" <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1996 May 25 0 WallClock )
            , ( ZoneState 390 (Save 0), DateTime 1996 Oct 26 30 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 2006 Apr 15 30 WallClock )
            ]
            (ZoneState 330 (Save 0))


{-| `Asia/Damascus`
-}
asia__damascus : () -> Time2.Zone
asia__damascus _ =
    fromSpecification "Asia/Damascus" <|
        Zone
            []
            (ZoneState 120 (Rules rules_Syria))


{-| `Asia/Dhaka`
-}
asia__dhaka : () -> Time2.Zone
asia__dhaka _ =
    fromSpecification "Asia/Dhaka" <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 2009 Jan 1 0 WallClock )
            ]
            (ZoneState 360 (Rules rules_Dhaka))


{-| `Asia/Dili`
-}
asia__dili : () -> Time2.Zone
asia__dili _ =
    fromSpecification "Asia/Dili" <|
        Zone
            [ ( ZoneState 540 (Save 0), DateTime 1976 May 3 0 WallClock )
            , ( ZoneState 480 (Save 0), DateTime 2000 Sep 17 0 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Dubai`
-}
asia__dubai : () -> Time2.Zone
asia__dubai _ =
    fromSpecification "Asia/Dubai" <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Asia/Dushanbe`
-}
asia__dushanbe : () -> Time2.Zone
asia__dushanbe _ =
    fromSpecification "Asia/Dushanbe" <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Save 60), DateTime 1991 Sep 9 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Famagusta`
-}
asia__famagusta : () -> Time2.Zone
asia__famagusta _ =
    fromSpecification "Asia/Famagusta" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Cyprus), DateTime 1998 Sep 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_EUAsia), DateTime 2016 Sep 8 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 2017 Oct 29 60 Universal )
            ]
            (ZoneState 120 (Rules rules_EUAsia))


{-| `Asia/Gaza`
-}
asia__gaza : () -> Time2.Zone
asia__gaza _ =
    fromSpecification "Asia/Gaza" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Zion), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Jordan), DateTime 1999 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2008 Aug 29 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2008 Sep 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2010 Jan 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2010 Mar 27 1 WallClock )
            , ( ZoneState 120 (Rules rules_Palestine), DateTime 2011 Aug 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2012 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Palestine))


{-| `Asia/Hebron`
-}
asia__hebron : () -> Time2.Zone
asia__hebron _ =
    fromSpecification "Asia/Hebron" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Zion), DateTime 1996 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Jordan), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Palestine))


{-| `Asia/Ho_Chi_Minh`
-}
asia__ho_chi_minh : () -> Time2.Zone
asia__ho_chi_minh _ =
    fromSpecification "Asia/Ho_Chi_Minh" <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 1975 Jun 13 0 WallClock )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Hong_Kong`
-}
asia__hong_kong : () -> Time2.Zone
asia__hong_kong _ =
    fromSpecification "Asia/Hong_Kong" <|
        Zone
            []
            (ZoneState 480 (Rules rules_HK))


{-| `Asia/Hovd`
-}
asia__hovd : () -> Time2.Zone
asia__hovd _ =
    fromSpecification "Asia/Hovd" <|
        Zone
            [ ( ZoneState 360 (Save 0), DateTime 1978 Jan 1 0 WallClock )
            ]
            (ZoneState 420 (Rules rules_Mongol))


{-| `Asia/Irkutsk`
-}
asia__irkutsk : () -> Time2.Zone
asia__irkutsk _ =
    fromSpecification "Asia/Irkutsk" <|
        Zone
            [ ( ZoneState 480 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 540 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Jakarta`
-}
asia__jakarta : () -> Time2.Zone
asia__jakarta _ =
    fromSpecification "Asia/Jakarta" <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Asia/Jayapura`
-}
asia__jayapura : () -> Time2.Zone
asia__jayapura _ =
    fromSpecification "Asia/Jayapura" <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Asia/Jerusalem`
-}
asia__jerusalem : () -> Time2.Zone
asia__jerusalem _ =
    fromSpecification "Asia/Jerusalem" <|
        Zone
            []
            (ZoneState 120 (Rules rules_Zion))


{-| `Asia/Kabul`
-}
asia__kabul : () -> Time2.Zone
asia__kabul _ =
    fromSpecification "Asia/Kabul" <|
        Zone
            []
            (ZoneState 270 (Save 0))


{-| `Asia/Kamchatka`
-}
asia__kamchatka : () -> Time2.Zone
asia__kamchatka _ =
    fromSpecification "Asia/Kamchatka" <|
        Zone
            [ ( ZoneState 720 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 720 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 720 (Save 0))


{-| `Asia/Karachi`
-}
asia__karachi : () -> Time2.Zone
asia__karachi _ =
    fromSpecification "Asia/Karachi" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1971 Mar 26 0 WallClock )
            ]
            (ZoneState 300 (Rules rules_Pakistan))


{-| `Asia/Kathmandu`
-}
asia__kathmandu : () -> Time2.Zone
asia__kathmandu _ =
    fromSpecification "Asia/Kathmandu" <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState 345 (Save 0))


{-| `Asia/Khandyga`
-}
asia__khandyga : () -> Time2.Zone
asia__khandyga _ =
    fromSpecification "Asia/Khandyga" <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2004 Jan 1 0 WallClock )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2011 Sep 13 0 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Kolkata`
-}
asia__kolkata : () -> Time2.Zone
asia__kolkata _ =
    fromSpecification "Asia/Kolkata" <|
        Zone
            []
            (ZoneState 330 (Save 0))


{-| `Asia/Krasnoyarsk`
-}
asia__krasnoyarsk : () -> Time2.Zone
asia__krasnoyarsk _ =
    fromSpecification "Asia/Krasnoyarsk" <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 480 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Kuala_Lumpur`
-}
asia__kuala_lumpur : () -> Time2.Zone
asia__kuala_lumpur _ =
    fromSpecification "Asia/Kuala_Lumpur" <|
        Zone
            [ ( ZoneState 450 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Kuching`
-}
asia__kuching : () -> Time2.Zone
asia__kuching _ =
    fromSpecification "Asia/Kuching" <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Macau`
-}
asia__macau : () -> Time2.Zone
asia__macau _ =
    fromSpecification "Asia/Macau" <|
        Zone
            []
            (ZoneState 480 (Rules rules_Macau))


{-| `Asia/Magadan`
-}
asia__magadan : () -> Time2.Zone
asia__magadan _ =
    fromSpecification "Asia/Magadan" <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2016 Apr 24 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Makassar`
-}
asia__makassar : () -> Time2.Zone
asia__makassar _ =
    fromSpecification "Asia/Makassar" <|
        Zone
            []
            (ZoneState 480 (Save 0))


{-| `Asia/Manila`
-}
asia__manila : () -> Time2.Zone
asia__manila _ =
    fromSpecification "Asia/Manila" <|
        Zone
            []
            (ZoneState 480 (Rules rules_Phil))


{-| `Asia/Nicosia`
-}
asia__nicosia : () -> Time2.Zone
asia__nicosia _ =
    fromSpecification "Asia/Nicosia" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Cyprus), DateTime 1998 Sep 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EUAsia))


{-| `Asia/Novokuznetsk`
-}
asia__novokuznetsk : () -> Time2.Zone
asia__novokuznetsk _ =
    fromSpecification "Asia/Novokuznetsk" <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Novosibirsk`
-}
asia__novosibirsk : () -> Time2.Zone
asia__novosibirsk _ =
    fromSpecification "Asia/Novosibirsk" <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 1993 May 23 0 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 Jul 24 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Omsk`
-}
asia__omsk : () -> Time2.Zone
asia__omsk _ =
    fromSpecification "Asia/Omsk" <|
        Zone
            [ ( ZoneState 360 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 300 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Oral`
-}
asia__oral : () -> Time2.Zone
asia__oral _ =
    fromSpecification "Asia/Oral" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Pontianak`
-}
asia__pontianak : () -> Time2.Zone
asia__pontianak _ =
    fromSpecification "Asia/Pontianak" <|
        Zone
            [ ( ZoneState 480 (Save 0), DateTime 1988 Jan 1 0 WallClock )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Pyongyang`
-}
asia__pyongyang : () -> Time2.Zone
asia__pyongyang _ =
    fromSpecification "Asia/Pyongyang" <|
        Zone
            [ ( ZoneState 540 (Save 0), DateTime 2015 Aug 15 0 WallClock )
            , ( ZoneState 510 (Save 0), DateTime 2018 May 4 1410 WallClock )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Qatar`
-}
asia__qatar : () -> Time2.Zone
asia__qatar _ =
    fromSpecification "Asia/Qatar" <|
        Zone
            [ ( ZoneState 240 (Save 0), DateTime 1972 Jun 1 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Asia/Qostanay`
-}
asia__qostanay : () -> Time2.Zone
asia__qostanay _ =
    fromSpecification "Asia/Qostanay" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Qyzylorda`
-}
asia__qyzylorda : () -> Time2.Zone
asia__qyzylorda _ =
    fromSpecification "Asia/Qyzylorda" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 2004 Oct 31 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2018 Dec 21 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Riyadh`
-}
asia__riyadh : () -> Time2.Zone
asia__riyadh _ =
    fromSpecification "Asia/Riyadh" <|
        Zone
            []
            (ZoneState 180 (Save 0))


{-| `Asia/Sakhalin`
-}
asia__sakhalin : () -> Time2.Zone
asia__sakhalin _ =
    fromSpecification "Asia/Sakhalin" <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1997 Mar 30 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Samarkand`
-}
asia__samarkand : () -> Time2.Zone
asia__samarkand _ =
    fromSpecification "Asia/Samarkand" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 300 (Save 60), DateTime 1981 Oct 1 0 WallClock )
            , ( ZoneState 360 (Save 0), DateTime 1982 Apr 1 0 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Seoul`
-}
asia__seoul : () -> Time2.Zone
asia__seoul _ =
    fromSpecification "Asia/Seoul" <|
        Zone
            []
            (ZoneState 540 (Rules rules_ROK))


{-| `Asia/Shanghai`
-}
asia__shanghai : () -> Time2.Zone
asia__shanghai _ =
    fromSpecification "Asia/Shanghai" <|
        Zone
            []
            (ZoneState 480 (Rules rules_PRC))


{-| `Asia/Singapore`
-}
asia__singapore : () -> Time2.Zone
asia__singapore _ =
    fromSpecification "Asia/Singapore" <|
        Zone
            [ ( ZoneState 450 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            ]
            (ZoneState 480 (Save 0))


{-| `Asia/Srednekolymsk`
-}
asia__srednekolymsk : () -> Time2.Zone
asia__srednekolymsk _ =
    fromSpecification "Asia/Srednekolymsk" <|
        Zone
            [ ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 660 (Save 0))


{-| `Asia/Taipei`
-}
asia__taipei : () -> Time2.Zone
asia__taipei _ =
    fromSpecification "Asia/Taipei" <|
        Zone
            []
            (ZoneState 480 (Rules rules_Taiwan))


{-| `Asia/Tashkent`
-}
asia__tashkent : () -> Time2.Zone
asia__tashkent _ =
    fromSpecification "Asia/Tashkent" <|
        Zone
            [ ( ZoneState 360 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 300 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Tbilisi`
-}
asia__tbilisi : () -> Time2.Zone
asia__tbilisi _ =
    fromSpecification "Asia/Tbilisi" <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState 180 (Rules rules_E_EurAsia), DateTime 1994 Sep 25 0 WallClock )
            , ( ZoneState 240 (Rules rules_E_EurAsia), DateTime 1996 Oct 27 0 WallClock )
            , ( ZoneState 240 (Save 60), DateTime 1997 Mar 30 0 WallClock )
            , ( ZoneState 240 (Rules rules_E_EurAsia), DateTime 2004 Jun 27 0 WallClock )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 2005 Mar 27 120 WallClock )
            ]
            (ZoneState 240 (Save 0))


{-| `Asia/Tehran`
-}
asia__tehran : () -> Time2.Zone
asia__tehran _ =
    fromSpecification "Asia/Tehran" <|
        Zone
            [ ( ZoneState 210 (Save 0), DateTime 1977 Nov 1 0 WallClock )
            , ( ZoneState 240 (Rules rules_Iran), DateTime 1979 Jan 1 0 WallClock )
            ]
            (ZoneState 210 (Rules rules_Iran))


{-| `Asia/Thimphu`
-}
asia__thimphu : () -> Time2.Zone
asia__thimphu _ =
    fromSpecification "Asia/Thimphu" <|
        Zone
            [ ( ZoneState 330 (Save 0), DateTime 1987 Oct 1 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Asia/Tokyo`
-}
asia__tokyo : () -> Time2.Zone
asia__tokyo _ =
    fromSpecification "Asia/Tokyo" <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Asia/Tomsk`
-}
asia__tomsk : () -> Time2.Zone
asia__tomsk _ =
    fromSpecification "Asia/Tomsk" <|
        Zone
            [ ( ZoneState 420 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 420 (Rules rules_Russia), DateTime 2002 May 1 180 WallClock )
            , ( ZoneState 360 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 420 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2016 May 29 120 Standard )
            ]
            (ZoneState 420 (Save 0))


{-| `Asia/Ulaanbaatar`
-}
asia__ulaanbaatar : () -> Time2.Zone
asia__ulaanbaatar _ =
    fromSpecification "Asia/Ulaanbaatar" <|
        Zone
            [ ( ZoneState 420 (Save 0), DateTime 1978 Jan 1 0 WallClock )
            ]
            (ZoneState 480 (Rules rules_Mongol))


{-| `Asia/Urumqi`
-}
asia__urumqi : () -> Time2.Zone
asia__urumqi _ =
    fromSpecification "Asia/Urumqi" <|
        Zone
            []
            (ZoneState 360 (Save 0))


{-| `Asia/Ust-Nera`
-}
asia__ust_nera : () -> Time2.Zone
asia__ust_nera _ =
    fromSpecification "Asia/Ust-Nera" <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1981 Apr 1 0 WallClock )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 660 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 720 (Save 0), DateTime 2011 Sep 13 0 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 600 (Save 0))


{-| `Asia/Vladivostok`
-}
asia__vladivostok : () -> Time2.Zone
asia__vladivostok _ =
    fromSpecification "Asia/Vladivostok" <|
        Zone
            [ ( ZoneState 600 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 600 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 600 (Save 0))


{-| `Asia/Yakutsk`
-}
asia__yakutsk : () -> Time2.Zone
asia__yakutsk _ =
    fromSpecification "Asia/Yakutsk" <|
        Zone
            [ ( ZoneState 540 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 480 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 540 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 600 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 540 (Save 0))


{-| `Asia/Yangon`
-}
asia__yangon : () -> Time2.Zone
asia__yangon _ =
    fromSpecification "Asia/Yangon" <|
        Zone
            []
            (ZoneState 390 (Save 0))


{-| `Asia/Yekaterinburg`
-}
asia__yekaterinburg : () -> Time2.Zone
asia__yekaterinburg _ =
    fromSpecification "Asia/Yekaterinburg" <|
        Zone
            [ ( ZoneState 300 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 300 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 360 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 300 (Save 0))


{-| `Asia/Yerevan`
-}
asia__yerevan : () -> Time2.Zone
asia__yerevan _ =
    fromSpecification "Asia/Yerevan" <|
        Zone
            [ ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 180 (Rules rules_RussiaAsia), DateTime 1995 Sep 24 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1997 Jan 1 0 WallClock )
            , ( ZoneState 240 (Rules rules_RussiaAsia), DateTime 2011 Jan 1 0 WallClock )
            ]
            (ZoneState 240 (Rules rules_Armenia))


{-| `Atlantic/Azores`
-}
atlantic__azores : () -> Time2.Zone
atlantic__azores _ =
    fromSpecification "Atlantic/Azores" <|
        Zone
            [ ( ZoneState -60 (Rules rules_Port), DateTime 1983 Sep 25 60 Standard )
            , ( ZoneState -60 (Rules rules_W_Eur), DateTime 1992 Sep 27 60 Standard )
            , ( ZoneState 0 (Rules rules_EU), DateTime 1993 Mar 28 60 Universal )
            ]
            (ZoneState -60 (Rules rules_EU))


{-| `Atlantic/Bermuda`
-}
atlantic__bermuda : () -> Time2.Zone
atlantic__bermuda _ =
    fromSpecification "Atlantic/Bermuda" <|
        Zone
            [ ( ZoneState -240 (Save 0), DateTime 1974 Apr 28 120 WallClock )
            , ( ZoneState -240 (Rules rules_Canada), DateTime 1976 Jan 1 0 WallClock )
            ]
            (ZoneState -240 (Rules rules_US))


{-| `Atlantic/Canary`
-}
atlantic__canary : () -> Time2.Zone
atlantic__canary _ =
    fromSpecification "Atlantic/Canary" <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1980 Apr 6 0 Standard )
            , ( ZoneState 0 (Save 60), DateTime 1980 Sep 28 60 Universal )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Cape_Verde`
-}
atlantic__cape_verde : () -> Time2.Zone
atlantic__cape_verde _ =
    fromSpecification "Atlantic/Cape_Verde" <|
        Zone
            [ ( ZoneState -120 (Save 0), DateTime 1975 Nov 25 120 WallClock )
            ]
            (ZoneState -60 (Save 0))


{-| `Atlantic/Faroe`
-}
atlantic__faroe : () -> Time2.Zone
atlantic__faroe _ =
    fromSpecification "Atlantic/Faroe" <|
        Zone
            [ ( ZoneState 0 (Save 0), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Madeira`
-}
atlantic__madeira : () -> Time2.Zone
atlantic__madeira _ =
    fromSpecification "Atlantic/Madeira" <|
        Zone
            [ ( ZoneState 0 (Rules rules_Port), DateTime 1983 Sep 25 60 Standard )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Atlantic/Reykjavik`
-}
atlantic__reykjavik : () -> Time2.Zone
atlantic__reykjavik _ =
    fromSpecification "Atlantic/Reykjavik" <|
        Zone
            []
            (ZoneState 0 (Save 0))


{-| `Atlantic/South_Georgia`
-}
atlantic__south_georgia : () -> Time2.Zone
atlantic__south_georgia _ =
    fromSpecification "Atlantic/South_Georgia" <|
        Zone
            []
            (ZoneState -120 (Save 0))


{-| `Atlantic/Stanley`
-}
atlantic__stanley : () -> Time2.Zone
atlantic__stanley _ =
    fromSpecification "Atlantic/Stanley" <|
        Zone
            [ ( ZoneState -240 (Rules rules_Falk), DateTime 1983 May 1 0 WallClock )
            , ( ZoneState -180 (Rules rules_Falk), DateTime 1985 Sep 15 0 WallClock )
            , ( ZoneState -240 (Rules rules_Falk), DateTime 2010 Sep 5 120 WallClock )
            ]
            (ZoneState -180 (Save 0))


{-| `Australia/Adelaide`
-}
australia__adelaide : () -> Time2.Zone
australia__adelaide _ =
    fromSpecification "Australia/Adelaide" <|
        Zone
            [ ( ZoneState 570 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 570 (Rules rules_AS))


{-| `Australia/Brisbane`
-}
australia__brisbane : () -> Time2.Zone
australia__brisbane _ =
    fromSpecification "Australia/Brisbane" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AQ))


{-| `Australia/Broken_Hill`
-}
australia__broken_hill : () -> Time2.Zone
australia__broken_hill _ =
    fromSpecification "Australia/Broken_Hill" <|
        Zone
            [ ( ZoneState 570 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState 570 (Rules rules_AN), DateTime 2000 Jan 1 0 WallClock )
            ]
            (ZoneState 570 (Rules rules_AS))


{-| `Australia/Darwin`
-}
australia__darwin : () -> Time2.Zone
australia__darwin _ =
    fromSpecification "Australia/Darwin" <|
        Zone
            []
            (ZoneState 570 (Save 0))


{-| `Australia/Eucla`
-}
australia__eucla : () -> Time2.Zone
australia__eucla _ =
    fromSpecification "Australia/Eucla" <|
        Zone
            []
            (ZoneState 525 (Rules rules_AW))


{-| `Australia/Hobart`
-}
australia__hobart : () -> Time2.Zone
australia__hobart _ =
    fromSpecification "Australia/Hobart" <|
        Zone
            []
            (ZoneState 600 (Rules rules_AT))


{-| `Australia/Lindeman`
-}
australia__lindeman : () -> Time2.Zone
australia__lindeman _ =
    fromSpecification "Australia/Lindeman" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            , ( ZoneState 600 (Rules rules_AQ), DateTime 1992 Jul 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_Holiday))


{-| `Australia/Lord_Howe`
-}
australia__lord_howe : () -> Time2.Zone
australia__lord_howe _ =
    fromSpecification "Australia/Lord_Howe" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1981 Mar 1 0 WallClock )
            , ( ZoneState 630 (Rules rules_LH), DateTime 1985 Jul 1 0 WallClock )
            ]
            (ZoneState 630 (Rules rules_LH))


{-| `Australia/Melbourne`
-}
australia__melbourne : () -> Time2.Zone
australia__melbourne _ =
    fromSpecification "Australia/Melbourne" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AV))


{-| `Australia/Perth`
-}
australia__perth : () -> Time2.Zone
australia__perth _ =
    fromSpecification "Australia/Perth" <|
        Zone
            []
            (ZoneState 480 (Rules rules_AW))


{-| `Australia/Sydney`
-}
australia__sydney : () -> Time2.Zone
australia__sydney _ =
    fromSpecification "Australia/Sydney" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 1971 Jan 1 0 WallClock )
            ]
            (ZoneState 600 (Rules rules_AN))


{-| `Europe/Amsterdam`
-}
europe__amsterdam : () -> Time2.Zone
europe__amsterdam _ =
    fromSpecification "Europe/Amsterdam" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Andorra`
-}
europe__andorra : () -> Time2.Zone
europe__andorra _ =
    fromSpecification "Europe/Andorra" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1985 Mar 31 120 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Astrakhan`
-}
europe__astrakhan : () -> Time2.Zone
europe__astrakhan _ =
    fromSpecification "Europe/Astrakhan" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Athens`
-}
europe__athens : () -> Time2.Zone
europe__athens _ =
    fromSpecification "Europe/Athens" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Greece), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Belgrade`
-}
europe__belgrade : () -> Time2.Zone
europe__belgrade _ =
    fromSpecification "Europe/Belgrade" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Nov 27 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Berlin`
-}
europe__berlin : () -> Time2.Zone
europe__berlin _ =
    fromSpecification "Europe/Berlin" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Brussels`
-}
europe__brussels : () -> Time2.Zone
europe__brussels _ =
    fromSpecification "Europe/Brussels" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Bucharest`
-}
europe__bucharest : () -> Time2.Zone
europe__bucharest _ =
    fromSpecification "Europe/Bucharest" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Romania), DateTime 1981 Mar 29 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1991 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_Romania), DateTime 1994 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Budapest`
-}
europe__budapest : () -> Time2.Zone
europe__budapest _ =
    fromSpecification "Europe/Budapest" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Hungary), DateTime 1984 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Chisinau`
-}
europe__chisinau : () -> Time2.Zone
europe__chisinau _ =
    fromSpecification "Europe/Chisinau" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 May 6 120 WallClock )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_Moldova))


{-| `Europe/Copenhagen`
-}
europe__copenhagen : () -> Time2.Zone
europe__copenhagen _ =
    fromSpecification "Europe/Copenhagen" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Dublin`
-}
europe__dublin : () -> Time2.Zone
europe__dublin _ =
    fromSpecification "Europe/Dublin" <|
        Zone
            []
            (ZoneState 60 (Rules rules_Eire))


{-| `Europe/Gibraltar`
-}
europe__gibraltar : () -> Time2.Zone
europe__gibraltar _ =
    fromSpecification "Europe/Gibraltar" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1982 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Helsinki`
-}
europe__helsinki : () -> Time2.Zone
europe__helsinki _ =
    fromSpecification "Europe/Helsinki" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Finland), DateTime 1983 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Istanbul`
-}
europe__istanbul : () -> Time2.Zone
europe__istanbul _ =
    fromSpecification "Europe/Istanbul" <|
        Zone
            [ ( ZoneState 120 (Rules rules_Turkey), DateTime 1978 Jun 29 0 WallClock )
            , ( ZoneState 180 (Rules rules_Turkey), DateTime 1984 Nov 1 120 WallClock )
            , ( ZoneState 120 (Rules rules_Turkey), DateTime 2007 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2011 Mar 27 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2011 Mar 28 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2014 Mar 30 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2014 Mar 31 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2015 Oct 25 60 Universal )
            , ( ZoneState 120 (Save 60), DateTime 2015 Nov 8 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2016 Sep 7 0 WallClock )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Kaliningrad`
-}
europe__kaliningrad : () -> Time2.Zone
europe__kaliningrad _ =
    fromSpecification "Europe/Kaliningrad" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 120 (Save 0))


{-| `Europe/Kiev`
-}
europe__kiev : () -> Time2.Zone
europe__kiev _ =
    fromSpecification "Europe/Kiev" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 120 (Save 60), DateTime 1991 Sep 29 180 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Kirov`
-}
europe__kirov : () -> Time2.Zone
europe__kirov _ =
    fromSpecification "Europe/Kirov" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Lisbon`
-}
europe__lisbon : () -> Time2.Zone
europe__lisbon _ =
    fromSpecification "Europe/Lisbon" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1976 Sep 26 60 WallClock )
            , ( ZoneState 0 (Rules rules_Port), DateTime 1983 Sep 25 60 Standard )
            , ( ZoneState 0 (Rules rules_W_Eur), DateTime 1992 Sep 27 60 Standard )
            , ( ZoneState 60 (Rules rules_EU), DateTime 1996 Mar 31 60 Universal )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Europe/London`
-}
europe__london : () -> Time2.Zone
europe__london _ =
    fromSpecification "Europe/London" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1971 Oct 31 120 Universal )
            , ( ZoneState 0 (Rules rules_GB_Eire), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 0 (Rules rules_EU))


{-| `Europe/Luxembourg`
-}
europe__luxembourg : () -> Time2.Zone
europe__luxembourg _ =
    fromSpecification "Europe/Luxembourg" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Madrid`
-}
europe__madrid : () -> Time2.Zone
europe__madrid _ =
    fromSpecification "Europe/Madrid" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Spain), DateTime 1979 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Malta`
-}
europe__malta : () -> Time2.Zone
europe__malta _ =
    fromSpecification "Europe/Malta" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Italy), DateTime 1973 Mar 31 0 WallClock )
            , ( ZoneState 60 (Rules rules_Malta), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Minsk`
-}
europe__minsk : () -> Time2.Zone
europe__minsk _ =
    fromSpecification "Europe/Minsk" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Monaco`
-}
europe__monaco : () -> Time2.Zone
europe__monaco _ =
    fromSpecification "Europe/Monaco" <|
        Zone
            [ ( ZoneState 60 (Rules rules_France), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Moscow`
-}
europe__moscow : () -> Time2.Zone
europe__moscow _ =
    fromSpecification "Europe/Moscow" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Oslo`
-}
europe__oslo : () -> Time2.Zone
europe__oslo _ =
    fromSpecification "Europe/Oslo" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Paris`
-}
europe__paris : () -> Time2.Zone
europe__paris _ =
    fromSpecification "Europe/Paris" <|
        Zone
            [ ( ZoneState 60 (Rules rules_France), DateTime 1977 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Prague`
-}
europe__prague : () -> Time2.Zone
europe__prague _ =
    fromSpecification "Europe/Prague" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1979 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Riga`
-}
europe__riga : () -> Time2.Zone
europe__riga _ =
    fromSpecification "Europe/Riga" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Save 60), DateTime 1989 Sep 24 120 Standard )
            , ( ZoneState 120 (Rules rules_Latvia), DateTime 1997 Jan 21 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2000 Feb 29 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2001 Jan 2 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Rome`
-}
europe__rome : () -> Time2.Zone
europe__rome _ =
    fromSpecification "Europe/Rome" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Italy), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Samara`
-}
europe__samara : () -> Time2.Zone
europe__samara _ =
    fromSpecification "Europe/Samara" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 1991 Oct 20 180 WallClock )
            , ( ZoneState 240 (Rules rules_Russia), DateTime 2010 Mar 28 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Saratov`
-}
europe__saratov : () -> Time2.Zone
europe__saratov _ =
    fromSpecification "Europe/Saratov" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1988 Mar 27 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Dec 4 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Simferopol`
-}
europe__simferopol : () -> Time2.Zone
europe__simferopol _ =
    fromSpecification "Europe/Simferopol" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1994 May 1 0 WallClock )
            , ( ZoneState 180 (Rules rules_E_Eur), DateTime 1996 Mar 31 0 Standard )
            , ( ZoneState 180 (Save 60), DateTime 1996 Oct 27 180 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1997 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1997 Mar 30 60 Universal )
            , ( ZoneState 120 (Rules rules_EU), DateTime 2014 Mar 30 120 WallClock )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Sofia`
-}
europe__sofia : () -> Time2.Zone
europe__sofia _ =
    fromSpecification "Europe/Sofia" <|
        Zone
            [ ( ZoneState 120 (Save 0), DateTime 1979 Mar 31 1380 WallClock )
            , ( ZoneState 120 (Rules rules_Bulg), DateTime 1982 Sep 26 180 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1991 Jan 1 0 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1997 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Stockholm`
-}
europe__stockholm : () -> Time2.Zone
europe__stockholm _ =
    fromSpecification "Europe/Stockholm" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1980 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Tallinn`
-}
europe__tallinn : () -> Time2.Zone
europe__tallinn _ =
    fromSpecification "Europe/Tallinn" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Save 60), DateTime 1989 Sep 24 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1998 Sep 22 0 WallClock )
            , ( ZoneState 120 (Rules rules_EU), DateTime 1999 Oct 31 240 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 2002 Feb 21 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Tirane`
-}
europe__tirane : () -> Time2.Zone
europe__tirane _ =
    fromSpecification "Europe/Tirane" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Albania), DateTime 1984 Jul 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Ulyanovsk`
-}
europe__ulyanovsk : () -> Time2.Zone
europe__ulyanovsk _ =
    fromSpecification "Europe/Ulyanovsk" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1992 Jan 19 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2016 Mar 27 120 Standard )
            ]
            (ZoneState 240 (Save 0))


{-| `Europe/Uzhgorod`
-}
europe__uzhgorod : () -> Time2.Zone
europe__uzhgorod _ =
    fromSpecification "Europe/Uzhgorod" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1990 Jan 1 0 WallClock )
            , ( ZoneState 180 (Save 0), DateTime 1990 Jul 1 120 WallClock )
            , ( ZoneState 60 (Save 0), DateTime 1991 Mar 31 180 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Vienna`
-}
europe__vienna : () -> Time2.Zone
europe__vienna _ =
    fromSpecification "Europe/Vienna" <|
        Zone
            [ ( ZoneState 60 (Rules rules_Austria), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Vilnius`
-}
europe__vilnius : () -> Time2.Zone
europe__vilnius _ =
    fromSpecification "Europe/Vilnius" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1989 Mar 26 120 Standard )
            , ( ZoneState 120 (Rules rules_Russia), DateTime 1991 Sep 29 120 Standard )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1998 Jan 1 0 WallClock )
            , ( ZoneState 120 (Save 0), DateTime 1998 Mar 29 60 Universal )
            , ( ZoneState 60 (Rules rules_EU), DateTime 1999 Oct 31 60 Universal )
            , ( ZoneState 120 (Save 0), DateTime 2003 Jan 1 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Volgograd`
-}
europe__volgograd : () -> Time2.Zone
europe__volgograd _ =
    fromSpecification "Europe/Volgograd" <|
        Zone
            [ ( ZoneState 240 (Rules rules_Russia), DateTime 1988 Mar 27 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 1992 Mar 29 120 Standard )
            , ( ZoneState 180 (Rules rules_Russia), DateTime 2011 Mar 27 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2014 Oct 26 120 Standard )
            , ( ZoneState 180 (Save 0), DateTime 2018 Oct 28 120 Standard )
            , ( ZoneState 240 (Save 0), DateTime 2020 Dec 27 120 Standard )
            ]
            (ZoneState 180 (Save 0))


{-| `Europe/Warsaw`
-}
europe__warsaw : () -> Time2.Zone
europe__warsaw _ =
    fromSpecification "Europe/Warsaw" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1977 Jan 1 0 WallClock )
            , ( ZoneState 60 (Rules rules_W_Eur), DateTime 1988 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Europe/Zaporozhye`
-}
europe__zaporozhye : () -> Time2.Zone
europe__zaporozhye _ =
    fromSpecification "Europe/Zaporozhye" <|
        Zone
            [ ( ZoneState 180 (Rules rules_Russia), DateTime 1991 Mar 31 120 WallClock )
            , ( ZoneState 120 (Rules rules_E_Eur), DateTime 1992 Mar 20 0 WallClock )
            , ( ZoneState 120 (Rules rules_C_Eur), DateTime 1996 May 13 0 WallClock )
            ]
            (ZoneState 120 (Rules rules_EU))


{-| `Europe/Zurich`
-}
europe__zurich : () -> Time2.Zone
europe__zurich _ =
    fromSpecification "Europe/Zurich" <|
        Zone
            [ ( ZoneState 60 (Save 0), DateTime 1981 Jan 1 0 WallClock )
            ]
            (ZoneState 60 (Rules rules_EU))


{-| `Indian/Chagos`
-}
indian__chagos : () -> Time2.Zone
indian__chagos _ =
    fromSpecification "Indian/Chagos" <|
        Zone
            [ ( ZoneState 300 (Save 0), DateTime 1996 Jan 1 0 WallClock )
            ]
            (ZoneState 360 (Save 0))


{-| `Indian/Christmas`
-}
indian__christmas : () -> Time2.Zone
indian__christmas _ =
    fromSpecification "Indian/Christmas" <|
        Zone
            []
            (ZoneState 420 (Save 0))


{-| `Indian/Cocos`
-}
indian__cocos : () -> Time2.Zone
indian__cocos _ =
    fromSpecification "Indian/Cocos" <|
        Zone
            []
            (ZoneState 390 (Save 0))


{-| `Indian/Kerguelen`
-}
indian__kerguelen : () -> Time2.Zone
indian__kerguelen _ =
    fromSpecification "Indian/Kerguelen" <|
        Zone
            []
            (ZoneState 300 (Save 0))


{-| `Indian/Mahe`
-}
indian__mahe : () -> Time2.Zone
indian__mahe _ =
    fromSpecification "Indian/Mahe" <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Indian/Maldives`
-}
indian__maldives : () -> Time2.Zone
indian__maldives _ =
    fromSpecification "Indian/Maldives" <|
        Zone
            []
            (ZoneState 300 (Save 0))


{-| `Indian/Mauritius`
-}
indian__mauritius : () -> Time2.Zone
indian__mauritius _ =
    fromSpecification "Indian/Mauritius" <|
        Zone
            []
            (ZoneState 240 (Rules rules_Mauritius))


{-| `Indian/Reunion`
-}
indian__reunion : () -> Time2.Zone
indian__reunion _ =
    fromSpecification "Indian/Reunion" <|
        Zone
            []
            (ZoneState 240 (Save 0))


{-| `Pacific/Apia`
-}
pacific__apia : () -> Time2.Zone
pacific__apia _ =
    fromSpecification "Pacific/Apia" <|
        Zone
            [ ( ZoneState -660 (Rules rules_WS), DateTime 2011 Dec 29 1440 WallClock )
            ]
            (ZoneState 780 (Rules rules_WS))


{-| `Pacific/Auckland`
-}
pacific__auckland : () -> Time2.Zone
pacific__auckland _ =
    fromSpecification "Pacific/Auckland" <|
        Zone
            []
            (ZoneState 720 (Rules rules_NZ))


{-| `Pacific/Bougainville`
-}
pacific__bougainville : () -> Time2.Zone
pacific__bougainville _ =
    fromSpecification "Pacific/Bougainville" <|
        Zone
            [ ( ZoneState 600 (Save 0), DateTime 2014 Dec 28 120 WallClock )
            ]
            (ZoneState 660 (Save 0))


{-| `Pacific/Chatham`
-}
pacific__chatham : () -> Time2.Zone
pacific__chatham _ =
    fromSpecification "Pacific/Chatham" <|
        Zone
            []
            (ZoneState 765 (Rules rules_Chatham))


{-| `Pacific/Chuuk`
-}
pacific__chuuk : () -> Time2.Zone
pacific__chuuk _ =
    fromSpecification "Pacific/Chuuk" <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Pacific/Easter`
-}
pacific__easter : () -> Time2.Zone
pacific__easter _ =
    fromSpecification "Pacific/Easter" <|
        Zone
            [ ( ZoneState -420 (Rules rules_Chile), DateTime 1982 Mar 14 180 Universal )
            ]
            (ZoneState -360 (Rules rules_Chile))


{-| `Pacific/Efate`
-}
pacific__efate : () -> Time2.Zone
pacific__efate _ =
    fromSpecification "Pacific/Efate" <|
        Zone
            []
            (ZoneState 660 (Rules rules_Vanuatu))


{-| `Pacific/Fakaofo`
-}
pacific__fakaofo : () -> Time2.Zone
pacific__fakaofo _ =
    fromSpecification "Pacific/Fakaofo" <|
        Zone
            [ ( ZoneState -660 (Save 0), DateTime 2011 Dec 30 0 WallClock )
            ]
            (ZoneState 780 (Save 0))


{-| `Pacific/Fiji`
-}
pacific__fiji : () -> Time2.Zone
pacific__fiji _ =
    fromSpecification "Pacific/Fiji" <|
        Zone
            []
            (ZoneState 720 (Rules rules_Fiji))


{-| `Pacific/Funafuti`
-}
pacific__funafuti : () -> Time2.Zone
pacific__funafuti _ =
    fromSpecification "Pacific/Funafuti" <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Galapagos`
-}
pacific__galapagos : () -> Time2.Zone
pacific__galapagos _ =
    fromSpecification "Pacific/Galapagos" <|
        Zone
            [ ( ZoneState -300 (Save 0), DateTime 1986 Jan 1 0 WallClock )
            ]
            (ZoneState -360 (Rules rules_Ecuador))


{-| `Pacific/Gambier`
-}
pacific__gambier : () -> Time2.Zone
pacific__gambier _ =
    fromSpecification "Pacific/Gambier" <|
        Zone
            []
            (ZoneState -540 (Save 0))


{-| `Pacific/Guadalcanal`
-}
pacific__guadalcanal : () -> Time2.Zone
pacific__guadalcanal _ =
    fromSpecification "Pacific/Guadalcanal" <|
        Zone
            []
            (ZoneState 660 (Save 0))


{-| `Pacific/Guam`
-}
pacific__guam : () -> Time2.Zone
pacific__guam _ =
    fromSpecification "Pacific/Guam" <|
        Zone
            [ ( ZoneState 600 (Rules rules_Guam), DateTime 2000 Dec 23 0 WallClock )
            ]
            (ZoneState 600 (Save 0))


{-| `Pacific/Honolulu`
-}
pacific__honolulu : () -> Time2.Zone
pacific__honolulu _ =
    fromSpecification "Pacific/Honolulu" <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Pacific/Kanton`
-}
pacific__kanton : () -> Time2.Zone
pacific__kanton _ =
    fromSpecification "Pacific/Kanton" <|
        Zone
            [ ( ZoneState -720 (Save 0), DateTime 1979 Oct 1 0 WallClock )
            , ( ZoneState -660 (Save 0), DateTime 1994 Dec 31 0 WallClock )
            ]
            (ZoneState 780 (Save 0))


{-| `Pacific/Kiritimati`
-}
pacific__kiritimati : () -> Time2.Zone
pacific__kiritimati _ =
    fromSpecification "Pacific/Kiritimati" <|
        Zone
            [ ( ZoneState -640 (Save 0), DateTime 1979 Oct 1 0 WallClock )
            , ( ZoneState -600 (Save 0), DateTime 1994 Dec 31 0 WallClock )
            ]
            (ZoneState 840 (Save 0))


{-| `Pacific/Kosrae`
-}
pacific__kosrae : () -> Time2.Zone
pacific__kosrae _ =
    fromSpecification "Pacific/Kosrae" <|
        Zone
            [ ( ZoneState 720 (Save 0), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 660 (Save 0))


{-| `Pacific/Kwajalein`
-}
pacific__kwajalein : () -> Time2.Zone
pacific__kwajalein _ =
    fromSpecification "Pacific/Kwajalein" <|
        Zone
            [ ( ZoneState -720 (Save 0), DateTime 1993 Aug 20 1440 WallClock )
            ]
            (ZoneState 720 (Save 0))


{-| `Pacific/Majuro`
-}
pacific__majuro : () -> Time2.Zone
pacific__majuro _ =
    fromSpecification "Pacific/Majuro" <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Marquesas`
-}
pacific__marquesas : () -> Time2.Zone
pacific__marquesas _ =
    fromSpecification "Pacific/Marquesas" <|
        Zone
            []
            (ZoneState -570 (Save 0))


{-| `Pacific/Nauru`
-}
pacific__nauru : () -> Time2.Zone
pacific__nauru _ =
    fromSpecification "Pacific/Nauru" <|
        Zone
            [ ( ZoneState 690 (Save 0), DateTime 1979 Feb 10 120 WallClock )
            ]
            (ZoneState 720 (Save 0))


{-| `Pacific/Niue`
-}
pacific__niue : () -> Time2.Zone
pacific__niue _ =
    fromSpecification "Pacific/Niue" <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Pacific/Norfolk`
-}
pacific__norfolk : () -> Time2.Zone
pacific__norfolk _ =
    fromSpecification "Pacific/Norfolk" <|
        Zone
            [ ( ZoneState 690 (Save 0), DateTime 1974 Oct 27 120 Standard )
            , ( ZoneState 690 (Save 60), DateTime 1975 Mar 2 120 Standard )
            , ( ZoneState 690 (Save 0), DateTime 2015 Oct 4 120 Standard )
            , ( ZoneState 660 (Save 0), DateTime 2019 Jul 1 0 WallClock )
            ]
            (ZoneState 660 (Rules rules_AN))


{-| `Pacific/Noumea`
-}
pacific__noumea : () -> Time2.Zone
pacific__noumea _ =
    fromSpecification "Pacific/Noumea" <|
        Zone
            []
            (ZoneState 660 (Rules rules_NC))


{-| `Pacific/Pago_Pago`
-}
pacific__pago_pago : () -> Time2.Zone
pacific__pago_pago _ =
    fromSpecification "Pacific/Pago_Pago" <|
        Zone
            []
            (ZoneState -660 (Save 0))


{-| `Pacific/Palau`
-}
pacific__palau : () -> Time2.Zone
pacific__palau _ =
    fromSpecification "Pacific/Palau" <|
        Zone
            []
            (ZoneState 540 (Save 0))


{-| `Pacific/Pitcairn`
-}
pacific__pitcairn : () -> Time2.Zone
pacific__pitcairn _ =
    fromSpecification "Pacific/Pitcairn" <|
        Zone
            [ ( ZoneState -510 (Save 0), DateTime 1998 Apr 27 0 WallClock )
            ]
            (ZoneState -480 (Save 0))


{-| `Pacific/Pohnpei`
-}
pacific__pohnpei : () -> Time2.Zone
pacific__pohnpei _ =
    fromSpecification "Pacific/Pohnpei" <|
        Zone
            []
            (ZoneState 660 (Save 0))


{-| `Pacific/Port_Moresby`
-}
pacific__port_moresby : () -> Time2.Zone
pacific__port_moresby _ =
    fromSpecification "Pacific/Port_Moresby" <|
        Zone
            []
            (ZoneState 600 (Save 0))


{-| `Pacific/Rarotonga`
-}
pacific__rarotonga : () -> Time2.Zone
pacific__rarotonga _ =
    fromSpecification "Pacific/Rarotonga" <|
        Zone
            [ ( ZoneState -630 (Save 0), DateTime 1978 Nov 12 0 WallClock )
            ]
            (ZoneState -600 (Rules rules_Cook))


{-| `Pacific/Tahiti`
-}
pacific__tahiti : () -> Time2.Zone
pacific__tahiti _ =
    fromSpecification "Pacific/Tahiti" <|
        Zone
            []
            (ZoneState -600 (Save 0))


{-| `Pacific/Tarawa`
-}
pacific__tarawa : () -> Time2.Zone
pacific__tarawa _ =
    fromSpecification "Pacific/Tarawa" <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Tongatapu`
-}
pacific__tongatapu : () -> Time2.Zone
pacific__tongatapu _ =
    fromSpecification "Pacific/Tongatapu" <|
        Zone
            [ ( ZoneState 780 (Save 0), DateTime 1999 Jan 1 0 WallClock )
            ]
            (ZoneState 780 (Rules rules_Tonga))


{-| `Pacific/Wake`
-}
pacific__wake : () -> Time2.Zone
pacific__wake _ =
    fromSpecification "Pacific/Wake" <|
        Zone
            []
            (ZoneState 720 (Save 0))


{-| `Pacific/Wallis`
-}
pacific__wallis : () -> Time2.Zone
pacific__wallis _ =
    fromSpecification "Pacific/Wallis" <|
        Zone
            []
            (ZoneState 720 (Save 0))



-- Links


{-| `Africa/Accra` (alias of `Africa/Abidjan`)
-}
africa__accra : () -> Time2.Zone
africa__accra =
    africa__abidjan


{-| `Africa/Addis_Ababa` (alias of `Africa/Nairobi`)
-}
africa__addis_ababa : () -> Time2.Zone
africa__addis_ababa =
    africa__nairobi


{-| `Africa/Asmara` (alias of `Africa/Nairobi`)
-}
africa__asmara : () -> Time2.Zone
africa__asmara =
    africa__nairobi


{-| `Africa/Bamako` (alias of `Africa/Abidjan`)
-}
africa__bamako : () -> Time2.Zone
africa__bamako =
    africa__abidjan


{-| `Africa/Bangui` (alias of `Africa/Lagos`)
-}
africa__bangui : () -> Time2.Zone
africa__bangui =
    africa__lagos


{-| `Africa/Banjul` (alias of `Africa/Abidjan`)
-}
africa__banjul : () -> Time2.Zone
africa__banjul =
    africa__abidjan


{-| `Africa/Blantyre` (alias of `Africa/Maputo`)
-}
africa__blantyre : () -> Time2.Zone
africa__blantyre =
    africa__maputo


{-| `Africa/Brazzaville` (alias of `Africa/Lagos`)
-}
africa__brazzaville : () -> Time2.Zone
africa__brazzaville =
    africa__lagos


{-| `Africa/Bujumbura` (alias of `Africa/Maputo`)
-}
africa__bujumbura : () -> Time2.Zone
africa__bujumbura =
    africa__maputo


{-| `Africa/Conakry` (alias of `Africa/Abidjan`)
-}
africa__conakry : () -> Time2.Zone
africa__conakry =
    africa__abidjan


{-| `Africa/Dakar` (alias of `Africa/Abidjan`)
-}
africa__dakar : () -> Time2.Zone
africa__dakar =
    africa__abidjan


{-| `Africa/Dar_es_Salaam` (alias of `Africa/Nairobi`)
-}
africa__dar_es_salaam : () -> Time2.Zone
africa__dar_es_salaam =
    africa__nairobi


{-| `Africa/Djibouti` (alias of `Africa/Nairobi`)
-}
africa__djibouti : () -> Time2.Zone
africa__djibouti =
    africa__nairobi


{-| `Africa/Douala` (alias of `Africa/Lagos`)
-}
africa__douala : () -> Time2.Zone
africa__douala =
    africa__lagos


{-| `Africa/Freetown` (alias of `Africa/Abidjan`)
-}
africa__freetown : () -> Time2.Zone
africa__freetown =
    africa__abidjan


{-| `Africa/Gaborone` (alias of `Africa/Maputo`)
-}
africa__gaborone : () -> Time2.Zone
africa__gaborone =
    africa__maputo


{-| `Africa/Harare` (alias of `Africa/Maputo`)
-}
africa__harare : () -> Time2.Zone
africa__harare =
    africa__maputo


{-| `Africa/Kampala` (alias of `Africa/Nairobi`)
-}
africa__kampala : () -> Time2.Zone
africa__kampala =
    africa__nairobi


{-| `Africa/Kigali` (alias of `Africa/Maputo`)
-}
africa__kigali : () -> Time2.Zone
africa__kigali =
    africa__maputo


{-| `Africa/Kinshasa` (alias of `Africa/Lagos`)
-}
africa__kinshasa : () -> Time2.Zone
africa__kinshasa =
    africa__lagos


{-| `Africa/Libreville` (alias of `Africa/Lagos`)
-}
africa__libreville : () -> Time2.Zone
africa__libreville =
    africa__lagos


{-| `Africa/Lome` (alias of `Africa/Abidjan`)
-}
africa__lome : () -> Time2.Zone
africa__lome =
    africa__abidjan


{-| `Africa/Luanda` (alias of `Africa/Lagos`)
-}
africa__luanda : () -> Time2.Zone
africa__luanda =
    africa__lagos


{-| `Africa/Lubumbashi` (alias of `Africa/Maputo`)
-}
africa__lubumbashi : () -> Time2.Zone
africa__lubumbashi =
    africa__maputo


{-| `Africa/Lusaka` (alias of `Africa/Maputo`)
-}
africa__lusaka : () -> Time2.Zone
africa__lusaka =
    africa__maputo


{-| `Africa/Malabo` (alias of `Africa/Lagos`)
-}
africa__malabo : () -> Time2.Zone
africa__malabo =
    africa__lagos


{-| `Africa/Maseru` (alias of `Africa/Johannesburg`)
-}
africa__maseru : () -> Time2.Zone
africa__maseru =
    africa__johannesburg


{-| `Africa/Mbabane` (alias of `Africa/Johannesburg`)
-}
africa__mbabane : () -> Time2.Zone
africa__mbabane =
    africa__johannesburg


{-| `Africa/Mogadishu` (alias of `Africa/Nairobi`)
-}
africa__mogadishu : () -> Time2.Zone
africa__mogadishu =
    africa__nairobi


{-| `Africa/Niamey` (alias of `Africa/Lagos`)
-}
africa__niamey : () -> Time2.Zone
africa__niamey =
    africa__lagos


{-| `Africa/Nouakchott` (alias of `Africa/Abidjan`)
-}
africa__nouakchott : () -> Time2.Zone
africa__nouakchott =
    africa__abidjan


{-| `Africa/Ouagadougou` (alias of `Africa/Abidjan`)
-}
africa__ouagadougou : () -> Time2.Zone
africa__ouagadougou =
    africa__abidjan


{-| `Africa/Porto-Novo` (alias of `Africa/Lagos`)
-}
africa__porto_novo : () -> Time2.Zone
africa__porto_novo =
    africa__lagos


{-| `America/Anguilla` (alias of `America/Puerto_Rico`)
-}
america__anguilla : () -> Time2.Zone
america__anguilla =
    america__puerto_rico


{-| `America/Antigua` (alias of `America/Puerto_Rico`)
-}
america__antigua : () -> Time2.Zone
america__antigua =
    america__puerto_rico


{-| `America/Aruba` (alias of `America/Puerto_Rico`)
-}
america__aruba : () -> Time2.Zone
america__aruba =
    america__puerto_rico


{-| `America/Atikokan` (alias of `America/Panama`)
-}
america__atikokan : () -> Time2.Zone
america__atikokan =
    america__panama


{-| `America/Blanc-Sablon` (alias of `America/Puerto_Rico`)
-}
america__blanc_sablon : () -> Time2.Zone
america__blanc_sablon =
    america__puerto_rico


{-| `America/Cayman` (alias of `America/Panama`)
-}
america__cayman : () -> Time2.Zone
america__cayman =
    america__panama


{-| `America/Creston` (alias of `America/Phoenix`)
-}
america__creston : () -> Time2.Zone
america__creston =
    america__phoenix


{-| `America/Curacao` (alias of `America/Puerto_Rico`)
-}
america__curacao : () -> Time2.Zone
america__curacao =
    america__puerto_rico


{-| `America/Dominica` (alias of `America/Puerto_Rico`)
-}
america__dominica : () -> Time2.Zone
america__dominica =
    america__puerto_rico


{-| `America/Grenada` (alias of `America/Puerto_Rico`)
-}
america__grenada : () -> Time2.Zone
america__grenada =
    america__puerto_rico


{-| `America/Guadeloupe` (alias of `America/Puerto_Rico`)
-}
america__guadeloupe : () -> Time2.Zone
america__guadeloupe =
    america__puerto_rico


{-| `America/Kralendijk` (alias of `America/Puerto_Rico`)
-}
america__kralendijk : () -> Time2.Zone
america__kralendijk =
    america__puerto_rico


{-| `America/Lower_Princes` (alias of `America/Puerto_Rico`)
-}
america__lower_princes : () -> Time2.Zone
america__lower_princes =
    america__puerto_rico


{-| `America/Marigot` (alias of `America/Puerto_Rico`)
-}
america__marigot : () -> Time2.Zone
america__marigot =
    america__puerto_rico


{-| `America/Montserrat` (alias of `America/Puerto_Rico`)
-}
america__montserrat : () -> Time2.Zone
america__montserrat =
    america__puerto_rico


{-| `America/Nassau` (alias of `America/Toronto`)
-}
america__nassau : () -> Time2.Zone
america__nassau =
    america__toronto


{-| `America/Port_of_Spain` (alias of `America/Puerto_Rico`)
-}
america__port_of_spain : () -> Time2.Zone
america__port_of_spain =
    america__puerto_rico


{-| `America/St_Barthelemy` (alias of `America/Puerto_Rico`)
-}
america__st_barthelemy : () -> Time2.Zone
america__st_barthelemy =
    america__puerto_rico


{-| `America/St_Kitts` (alias of `America/Puerto_Rico`)
-}
america__st_kitts : () -> Time2.Zone
america__st_kitts =
    america__puerto_rico


{-| `America/St_Lucia` (alias of `America/Puerto_Rico`)
-}
america__st_lucia : () -> Time2.Zone
america__st_lucia =
    america__puerto_rico


{-| `America/St_Thomas` (alias of `America/Puerto_Rico`)
-}
america__st_thomas : () -> Time2.Zone
america__st_thomas =
    america__puerto_rico


{-| `America/St_Vincent` (alias of `America/Puerto_Rico`)
-}
america__st_vincent : () -> Time2.Zone
america__st_vincent =
    america__puerto_rico


{-| `America/Tortola` (alias of `America/Puerto_Rico`)
-}
america__tortola : () -> Time2.Zone
america__tortola =
    america__puerto_rico


{-| `Antarctica/DumontDUrville` (alias of `Pacific/Port_Moresby`)
-}
antarctica__dumontdurville : () -> Time2.Zone
antarctica__dumontdurville =
    pacific__port_moresby


{-| `Antarctica/McMurdo` (alias of `Pacific/Auckland`)
-}
antarctica__mcmurdo : () -> Time2.Zone
antarctica__mcmurdo =
    pacific__auckland


{-| `Antarctica/Syowa` (alias of `Asia/Riyadh`)
-}
antarctica__syowa : () -> Time2.Zone
antarctica__syowa =
    asia__riyadh


{-| `Arctic/Longyearbyen` (alias of `Europe/Oslo`)
-}
arctic__longyearbyen : () -> Time2.Zone
arctic__longyearbyen =
    europe__oslo


{-| `Asia/Aden` (alias of `Asia/Riyadh`)
-}
asia__aden : () -> Time2.Zone
asia__aden =
    asia__riyadh


{-| `Asia/Bahrain` (alias of `Asia/Qatar`)
-}
asia__bahrain : () -> Time2.Zone
asia__bahrain =
    asia__qatar


{-| `Asia/Istanbul` (alias of `Europe/Istanbul`)
-}
asia__istanbul : () -> Time2.Zone
asia__istanbul =
    europe__istanbul


{-| `Asia/Kuwait` (alias of `Asia/Riyadh`)
-}
asia__kuwait : () -> Time2.Zone
asia__kuwait =
    asia__riyadh


{-| `Asia/Muscat` (alias of `Asia/Dubai`)
-}
asia__muscat : () -> Time2.Zone
asia__muscat =
    asia__dubai


{-| `Asia/Phnom_Penh` (alias of `Asia/Bangkok`)
-}
asia__phnom_penh : () -> Time2.Zone
asia__phnom_penh =
    asia__bangkok


{-| `Asia/Vientiane` (alias of `Asia/Bangkok`)
-}
asia__vientiane : () -> Time2.Zone
asia__vientiane =
    asia__bangkok


{-| `Atlantic/St_Helena` (alias of `Africa/Abidjan`)
-}
atlantic__st_helena : () -> Time2.Zone
atlantic__st_helena =
    africa__abidjan


{-| `Europe/Bratislava` (alias of `Europe/Prague`)
-}
europe__bratislava : () -> Time2.Zone
europe__bratislava =
    europe__prague


{-| `Europe/Busingen` (alias of `Europe/Zurich`)
-}
europe__busingen : () -> Time2.Zone
europe__busingen =
    europe__zurich


{-| `Europe/Guernsey` (alias of `Europe/London`)
-}
europe__guernsey : () -> Time2.Zone
europe__guernsey =
    europe__london


{-| `Europe/Isle_of_Man` (alias of `Europe/London`)
-}
europe__isle_of_man : () -> Time2.Zone
europe__isle_of_man =
    europe__london


{-| `Europe/Jersey` (alias of `Europe/London`)
-}
europe__jersey : () -> Time2.Zone
europe__jersey =
    europe__london


{-| `Europe/Ljubljana` (alias of `Europe/Belgrade`)
-}
europe__ljubljana : () -> Time2.Zone
europe__ljubljana =
    europe__belgrade


{-| `Europe/Mariehamn` (alias of `Europe/Helsinki`)
-}
europe__mariehamn : () -> Time2.Zone
europe__mariehamn =
    europe__helsinki


{-| `Europe/Nicosia` (alias of `Asia/Nicosia`)
-}
europe__nicosia : () -> Time2.Zone
europe__nicosia =
    asia__nicosia


{-| `Europe/Podgorica` (alias of `Europe/Belgrade`)
-}
europe__podgorica : () -> Time2.Zone
europe__podgorica =
    europe__belgrade


{-| `Europe/San_Marino` (alias of `Europe/Rome`)
-}
europe__san_marino : () -> Time2.Zone
europe__san_marino =
    europe__rome


{-| `Europe/Sarajevo` (alias of `Europe/Belgrade`)
-}
europe__sarajevo : () -> Time2.Zone
europe__sarajevo =
    europe__belgrade


{-| `Europe/Skopje` (alias of `Europe/Belgrade`)
-}
europe__skopje : () -> Time2.Zone
europe__skopje =
    europe__belgrade


{-| `Europe/Vaduz` (alias of `Europe/Zurich`)
-}
europe__vaduz : () -> Time2.Zone
europe__vaduz =
    europe__zurich


{-| `Europe/Vatican` (alias of `Europe/Rome`)
-}
europe__vatican : () -> Time2.Zone
europe__vatican =
    europe__rome


{-| `Europe/Zagreb` (alias of `Europe/Belgrade`)
-}
europe__zagreb : () -> Time2.Zone
europe__zagreb =
    europe__belgrade


{-| `Indian/Antananarivo` (alias of `Africa/Nairobi`)
-}
indian__antananarivo : () -> Time2.Zone
indian__antananarivo =
    africa__nairobi


{-| `Indian/Comoro` (alias of `Africa/Nairobi`)
-}
indian__comoro : () -> Time2.Zone
indian__comoro =
    africa__nairobi


{-| `Indian/Mayotte` (alias of `Africa/Nairobi`)
-}
indian__mayotte : () -> Time2.Zone
indian__mayotte =
    africa__nairobi


{-| `Pacific/Midway` (alias of `Pacific/Pago_Pago`)
-}
pacific__midway : () -> Time2.Zone
pacific__midway =
    pacific__pago_pago


{-| `Pacific/Saipan` (alias of `Pacific/Guam`)
-}
pacific__saipan : () -> Time2.Zone
pacific__saipan =
    pacific__guam
