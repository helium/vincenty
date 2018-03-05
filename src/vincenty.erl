-module(vincenty).

-type coordinate() :: {float(), float()}.

-export([distance/2, distance/3]).

-define(RADIUS_AT_EQUATOR, 6378137).
-define(FLATTENING_ELIPSOID, 1/298.257223563).
-define(RADIUS_AT_POLES, (1-?FLATTENING_ELIPSOID)*?RADIUS_AT_EQUATOR).
-define(MILES_PER_KILOMETER, 0.621371).
-define(MAX_ITERATIONS, 200).
-define(CONVERGENCE_THRESHOLD, 0.000000000001).

-spec distance(coordinate(), coordinate()) -> float().
distance(Point1, Point2) ->
    distance(Point1, Point2, false).

-spec distance(coordinate(), coordinate(), boolean()) -> float().
distance(Point1, Point2, Miles) ->
    ExactAns = case Miles of
              true -> miles(vincenty_inverse(Point1, Point2));
              false -> vincenty_inverse(Point1, Point2)
          end,
    {RoundedToSixDecimalPlaces, _Rest} = string:to_float(float_to_list(ExactAns, [{decimals, 6}])),
    RoundedToSixDecimalPlaces.

-spec vincenty_inverse(coordinate(), coordinate()) -> float() | {error, fail_to_converge}.
vincenty_inverse({Lat1, Long1}, {Lat2, Long2}) ->
    U1 = math:atan((1-?FLATTENING_ELIPSOID)*math:tan(radians(Lat1))),
    U2 = math:atan((1-?FLATTENING_ELIPSOID)*math:tan(radians(Lat2))),
    InitLambda = Lambda = radians(Long2 - Long1),
    SinU1 = math:sin(U1),
    CosU1 = math:cos(U1),
    SinU2 = math:sin(U2),
    CosU2 = math:cos(U2),

    % recurse till ?MAX_ITERATIONS
    approximate(InitLambda, Lambda, SinU1, CosU1, SinU2, CosU2, 0).

-spec approximate(float(), float(), float(), float(), float(), float(), non_neg_integer()) -> float() | {error, fail_to_converge}.
approximate(_InitLambda, _Lambda,  _SinU1, _CosU1, _SinU2, _CosU2, ?MAX_ITERATIONS) ->
    {error, fail_to_converge};
approximate(InitLambda, Lambda, SinU1, CosU1, SinU2, CosU2, Iteration) ->
    SinLambda = math:sin(Lambda),
    CosLambda = math:cos(Lambda),
    SinSigma = math:sqrt(math:pow(CosU2*SinLambda, 2) +  math:pow((CosU1 * SinU2 - SinU1 * CosU2 * CosLambda), 2)),

    case SinSigma of
        0.0 -> 0.0;
        _ ->
            CosSigma = SinU1 * SinU2 + CosU1 * CosU2 * CosLambda,
            Sigma = math:atan2(SinSigma, CosSigma),
            SinAlpha = CosU1 * CosU2 * SinLambda / SinSigma,
            CosSqAlpha = 1 - math:pow(SinAlpha, 2),

            Cos2SigmaM = case CosSqAlpha of
                             0.0 -> 0.0;
                             _ -> CosSigma - 2 * SinU1 * SinU2 / CosSqAlpha
                         end,
            C = (?FLATTENING_ELIPSOID / 16) * CosSqAlpha * (4 + ?FLATTENING_ELIPSOID - 3 * CosSqAlpha),
            NewLambda = InitLambda + (1 - C) * ?FLATTENING_ELIPSOID * SinAlpha * (Sigma + C * SinSigma * (Cos2SigmaM + C * CosSigma * (-1 + 2 * math:pow(Cos2SigmaM, 2)))),

            case abs(NewLambda - Lambda) < ?CONVERGENCE_THRESHOLD of
                true ->
                    % succesfful convergence
                    evaluate(CosSqAlpha, SinSigma, Cos2SigmaM, CosSigma, Sigma);
                false -> approximate(InitLambda, NewLambda, SinU1, CosU1, SinU2, CosU2, Iteration + 1)
            end
    end.

-spec evaluate(float(), float(), float(), float(), float()) -> float().
evaluate(CosSqAlpha, SinSigma, Cos2SigmaM, CosSigma, Sigma) ->
    Usq = CosSqAlpha * (math:pow(?RADIUS_AT_EQUATOR, 2) - math:pow(?RADIUS_AT_POLES, 2)) / math:pow(?RADIUS_AT_POLES, 2),
    A = 1 + Usq / 16384 * (4096 + Usq * (-768 + Usq * (320 - 175 * Usq))),
    B = (Usq / 1024) * (256 + Usq * (-128 + Usq * (74 - 47 * Usq))),
    DeltaSigma = B * SinSigma * (Cos2SigmaM + (B / 4) * (CosSigma * (-1 + 2 * math:pow(Cos2SigmaM, 2)) - (B / 6) * Cos2SigmaM * (-3 + 4 * math:pow(SinSigma, 2)) * (-3 + 4 * math:pow(Cos2SigmaM, 2)))),
    ?RADIUS_AT_POLES * A * (Sigma - DeltaSigma)/1000.

-spec radians(float()) -> float().
radians(Degree) ->
    Degree * math:pi()/180.

-spec miles(float()) -> float().
miles(DistanceInKm) ->
    ?MILES_PER_KILOMETER * DistanceInKm.
