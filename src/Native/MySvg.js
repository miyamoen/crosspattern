Elm.Native.MySvg = {};
Elm.Native.MySvg.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.MySvg = localRuntime.Native.MySvg || {};
    if (localRuntime.Native.MySvg.values)
    {
        return localRuntime.Native.MySvg.values;
    }

    var Color = Elm.Native.Color.make(localRuntime);

    return localRuntime.Native.Color.values = {
        toCss: Color.toCss
    };
};
