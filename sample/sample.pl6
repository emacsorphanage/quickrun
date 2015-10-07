#!/usr/bin/env perl6

class Sample {
    method hello(Str $name) {
        say "Hello $name";
    }
}

Sample.new.hello("world");
