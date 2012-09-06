guard :shell, all_on_start: true do
  watch /.*\.l?hs$/ do |m|
    puts "\n\n\nCompiling..."
    `ghc -package ghc -package-conf ./cabal-dev/packages-7.4.1.conf #{m[0]} && echo "Compiled!"`
  end
end
