# Examples using the Yauhau IO optimization

## Blog

The blog example (`blog.hs` for the Haskell version and `blog.clj` for the (not currently runnable) clojure version) are adaptations of Simon Marlow's blog example he showed at ICFP14.
The original source code for Marlow's version can be found [here](https://github.com/simonmar/haxl-icfp14-sample-code/blob/master/blog.hs).

Our Haskell version is functionally identical, it only adds some `simpleLift` and `call` calls to run in [ohua](https://github.com/ohua-dev/ohua-core). 


### Running the Haskell example

1. Install the Haskell build tool `stack`

	You can find instructions appropriate for your platform on the [documentation web-site](https://docs.haskellstack.org) for `stack`.

2. Clone the repository

	`git clone https://github.com/ohua-dev/yauhau.git`
	
3. Build the project

	`stack build --fast`
	
	This command will download the necessary libraries and build the executable.
	
4. Run the executable

	`stack exec -- yauhau-blog-example`
	
The actual compiled binary is found in `<repo>/.stack-work/install/<platform>/<stack-snapshot>/<ghc-version>/bin/yauhau-blog-example`.
To obtain the concrete path for your system you can simply run `stack exec -- which yauhau-blog-example` after having compiled the project.

If you want to experiment with the example you can find the source in `examples/blog.hs` and this readme at `examples/README.md`.

### Running the clojure example

The source for the clojure version of the blog example is found at `examples/blog.clj`.

Unfortunately due to a bug in the platform we use to bring `ohua` to the JVM this example does not currently run.
Furthermore due to the bug we are unable to debug the example at the moment, its code is therefore most likely buggy to some extent.
