defmodule Bbmustache.Mixfile do
    use Mix.Project

    def project do
        [
         app:         :bbmustache,
         version:     "1.0.0",
         description: description,
         package:     package,
         deps:        deps
        ]
    end

    defp deps do
    end

    defp description do
        """
        Binary pattern match Based Mustache template engine for Erlang/OTP.
        """
    end

    defp package do
        [
         files:        ~w(src rebar.config README.md LICENSE),
         contributors: ["Hinagiku Soranoba"],
         licenses:     ["MIT"],
         links:        %{"GitHub" => "https://github.com/soranoba/bbmustache"}
        ]
    end
end
