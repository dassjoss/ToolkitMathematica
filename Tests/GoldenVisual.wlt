BeginTestSection["Visual Golden Suite"]

VerificationTest[
  TensorToolkit`$IndexRegistry = <|"mu"-><||>, "nu"-><||>, "alpha"-><||>, "beta"-><||>|>;
  SetTensorFormatting[R];
  ToString[InputForm[ToBoxes[R[mu, -nu, -alpha, beta], StandardForm]]],
  "TagBox[RowBox[{SubsuperscriptBox[\"R\", \" \", RowBox[{\"μ\"}]], SubsuperscriptBox[\" \", RowBox[{\"ν\", \"α\"}], \" \"], SubsuperscriptBox[\" \", \" \", RowBox[{\"β\"}]]}], \"Tensor\"]",
  TestID -> "Visual-Riemann"
]

VerificationTest[
  TensorToolkit`$IndexRegistry = <|"mu"-><||>, "nu"-><||>|>;
  SetTensorFormatting[g];
  ToString[InputForm[ToBoxes[g[-mu, -nu], StandardForm]]],
  "TagBox[RowBox[{SubsuperscriptBox[\"g\", RowBox[{\"μ\", \"ν\"}], \" \"]}], \"Tensor\"]",
  TestID -> "Visual-Metric"
]

VerificationTest[
  TensorToolkit`$IndexRegistry = <|"II"-><|"Manifold"->"Internal"|>, "mu"-><||>|>;
  SetTensorFormatting[e];
  ToString[InputForm[ToBoxes[e[-II, mu], StandardForm]]],
  "TagBox[RowBox[{SubsuperscriptBox[\"e\", RowBox[{StyleBox[\"\\\\[CapitalIota]\", Bold, FontSlant -> \"Plain\"]}], \" \"], SubsuperscriptBox[\" \", \" \", RowBox[{\"μ\"}]]}], \"Tensor\"]",
  TestID -> "Visual-Tetrad"
]

EndTestSection[]