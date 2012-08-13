class HelloEditor {
    var _editors : string[];

    function constructor () {
    }

    function constructor (editors : string[]) {
        this._editors = editors;
    }

    function hello () : void {
        this._editors.forEach(function(e) {
            log e;
        });
    }
}

class _Main {
    static function main(args :string[]) : void {
        var he = new HelloEditor(['Emacs', 'JSX']);
        he.hello();
    }
}
