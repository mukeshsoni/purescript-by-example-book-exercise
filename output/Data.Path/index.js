"use strict";
var Directory = (function () {
    function Directory(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Directory.create = function (value0) {
        return function (value1) {
            return new Directory(value0, value1);
        };
    };
    return Directory;
})();
var File = (function () {
    function File(value0) {
        this.value0 = value0;
    };
    File.create = function (value0) {
        return new File(value0);
    };
    return File;
})();
module.exports = {
    Directory: Directory,
    File: File
};
