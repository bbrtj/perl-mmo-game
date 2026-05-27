{
	# TODO: reuse logger from DI

	controllers => [qw(Bridges Main User)],

	modules => {
		Template => {
			paths => ['views'],
			conf => {
				OUTLINE_TAG => qr{\V*%%},
				EVAL_PERL => 1,
			},
		},
		Middleware => {
			Session => {
				secret => app->secrets,
				store => app->session_object,
			},
		},
	},
}

