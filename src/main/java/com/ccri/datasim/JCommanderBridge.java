package com.ccri.datasim;

import com.beust.jcommander.JCommander;

public class JCommanderBridge {
    public static JCommander create(Object argsObject) {
        return new JCommander(argsObject);
    }
}
