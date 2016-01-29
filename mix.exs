defmodule EBus.Mixfile do
  use Mix.Project

  def project do
    [app: :ebus,
     version: "0.3.0",
     compilers: [:erlang, :app],
     erlc_options: [:debug_info, :warnings_as_errors],
     deps: deps]
  end

  defp deps do
    [{:phoenix_pubsub, github: "phoenixframework/phoenix_pubsub", branch: "master"}]
  end
end
