# environment to store credentials
.state <- new.env(parent = emptyenv())

#' Authenticate to MS Dynamics CRM
#' 
#' Authenticate to an On-Premise IFD MS Dynamics CRM environment using a url, 
#' username, and password.
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom base64enc base64encode base64decode
#' @importFrom digest digest hmac
#' @importFrom httr GET POST add_headers status_code content
#' @importFrom XML newXMLNode setXMLNamespace saveXML
#' @importFrom xml2 xml_ns_strip xml_find_first xml_text read_xml
#' @param url login url
#' @param username username, typically an email address. A domain can be included 
#' (e.g. d03\\myemail).
#' @param password password
#' @param soap_path a custom SOAP path; defaults to "XRMServices/2011/Organization.svc"
#' @template verbose
#' @examples
#' \dontrun{
#' dyn_auth(url = "https://test.ztcrm.org/",
#'          username = "test@@live.com", 
#'          password = "{PASSWORD_HERE}")
#' }
#' @export
dyn_auth <- function(url = NULL,
                     username = NULL,
                     password = NULL,
                     soap_path = getOption("rdynamicscrm.soap_path"),
                     verbose = FALSE){
  
  stopifnot((!is.null(url) & !is.null(username) & !is.null(password)))
  
  # get login url --------------------------------------------------------------
  
  urn_address <- paste0(url, soap_path)
  resp <- GET(paste0(url, soap_path, '?wsdl=wsdl0'))
  login_url <- paste0(content(resp) %>%
                        xml_ns_strip() %>%
                        xml_find_first("wsp:Policy") %>%
                        xml_find_first("wsp:ExactlyOne") %>%
                        xml_find_first("wsp:All") %>%
                        xml_find_first("ms-xrm:AuthenticationPolicy") %>%
                        xml_find_first("ms-xrm:SecureTokenService") %>%
                        xml_find_first("ms-xrm:Identifier") %>%
                        xml_text(), '/13/usernamemixed')
  
  # build login request --------------------------------------------------------
  
  envelope <- newXMLNode("s:Envelope", 
                         namespaceDefinitions = c("a" = "http://www.w3.org/2005/08/addressing", 
                                                  "s" = "http://www.w3.org/2003/05/soap-envelope"))
  header <- newXMLNode("s:Header", parent=envelope)
  invisible(newXMLNode("a:Action", "http://docs.oasis-open.org/ws-sx/ws-trust/200512/RST/Issue", 
                       attrs=c(`s:mustUnderstand`="1"), parent=header))
  invisible(newXMLNode("a:MessageID", paste0("urn:uuid:", UUIDgenerate()), parent=header))
  invisible(newXMLNode("a:ReplyTo", 
                       newXMLNode("a:Address", "http://www.w3.org/2005/08/addressing/anonymous"), 
                       parent=header))
  security <- newXMLNode("Security", 
                         attrs=c(`s:mustUnderstand`="1"),
                         namespaceDefinitions = c("http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd", 
                                                  "u" = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"), 
                         parent=header)
  timestamp <- newXMLNode("u:Timestamp", attrs=c(`u:Id`=UUIDgenerate()), parent=security)
  creatednode <- newXMLNode("u:Created", 
                            strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"),
                            parent=timestamp)
  expiresnode <- newXMLNode("u:Expires", 
                            strftime(as.POSIXlt(Sys.time()+(60*60), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"), 
                            parent=timestamp)
  invisible(setXMLNamespace(creatednode, "u"))
  invisible(setXMLNamespace(expiresnode, "u"))
  invisible(setXMLNamespace(timestamp, "u"))
  usernametoken <- newXMLNode("UsernameToken", attrs=c(`u:Id`=UUIDgenerate()), parent=security)
  invisible(newXMLNode("Username", username,
                       parent=usernametoken))
  invisible(newXMLNode("Password", password, 
                       attrs=c(`Type`="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText"), 
                       parent=usernametoken))
  invisible(newXMLNode("a:To", login_url, 
                       attrs=c(`s:mustUnderstand`="1"), parent=header))
  body <- newXMLNode("s:Body", parent=envelope)
  requesttoken <- newXMLNode("trust:RequestSecurityToken", 
                             namespaceDefinitions = c("trust" = "http://docs.oasis-open.org/ws-sx/ws-trust/200512"), 
                             parent=body)
  appliesto <- newXMLNode("wsp:AppliesTo", 
                          namespaceDefinitions = c("wsp" = "http://schemas.xmlsoap.org/ws/2004/09/policy"), 
                          parent=requesttoken)
  invisible(newXMLNode("a:EndpointReference", 
                       newXMLNode("a:Address", urn_address), parent=appliesto))
  invisible(newXMLNode("trust:RequestType", 
                       "http://docs.oasis-open.org/ws-sx/ws-trust/200512/Issue",
                       parent=requesttoken))

  # send login request ---------------------------------------------------------
  
  body_text <- saveXML(envelope, encoding="UTF-8", indent=FALSE)
  httr_response <- POST(url = login_url,
                        add_headers(`Content-Type` = "application/soap+xml; charset=utf-8"),
                        body = body_text)
  catch_errors(httr_response)
  response_parsed <- content(httr_response, as="parsed", type="text/xml", encoding="UTF-8")
  
  # define header constants ------------------------------------------------------

  x509_issuer_name <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedSecurityToken//KeyInfo//e:EncryptedKey//KeyInfo//o:SecurityTokenReference//X509Data//X509IssuerSerial//X509IssuerName") %>%
    xml_text()
  
  x509_serial_number <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedSecurityToken//KeyInfo//e:EncryptedKey//KeyInfo//o:SecurityTokenReference//X509Data//X509IssuerSerial//X509SerialNumber") %>%
    xml_text()
    
  binary_secret <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedProofToken//trust:BinarySecret") %>%
    xml_text()
  
  key_identifier <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedAttachedReference//o:KeyIdentifier") %>%
    xml_text()

  ciphervalue1 <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedSecurityToken//xenc:EncryptedData//e:CipherValue") %>% 
    .[[1]] %>%
    xml_text()
  
  ciphervalue2 <- response_parsed %>% 
    xml_ns_strip() %>%
    xml_find_all("s:Body//trust:RequestedSecurityToken//xenc:EncryptedData//e:CipherValue") %>% 
    .[[2]] %>%
    xml_text()
  
  created <- strftime(as.POSIXlt(Sys.time()-(1*60), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z")
  expires <- strftime(as.POSIXlt(Sys.time()+(60*60), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z")
  
  t2 <- newXMLNode("u:Timestamp", 
                   attrs=c(`u:Id`="_0"),
                   namespaceDefinitions = c("u" = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"))
  invisible(newXMLNode("u:Created", 
                       created,
                       parent=t2))
  invisible(newXMLNode("u:Expires", 
                       expires,
                       parent=t2))
  
  timestamp_string <- saveXML(t2, encoding = "UTF-8", indent=FALSE)
  sha_object <- digest(charToRaw(enc2utf8(timestamp_string)), algo="sha1", serialize=FALSE, raw=TRUE)
  digest_value <- base64encode(sha_object)
  signedinfo <- paste0("<SignedInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></CanonicalizationMethod><SignatureMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#hmac-sha1\"></SignatureMethod><Reference URI=\"#_0\"><Transforms><Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"></Transform></Transforms><DigestMethod Algorithm=\"http://www.w3.org/2000/09/xmldsig#sha1\"></DigestMethod><DigestValue>", 
                       digest_value, "</DigestValue></Reference></SignedInfo>")
  signature_value <- base64encode(hmac(key=base64decode(binary_secret), 
                                       object=charToRaw(enc2utf8(signedinfo)), 
                                       algo="sha1", raw=TRUE))
  
  # build header ---------------------------------------------------------------
  
  header_w_envl <- newXMLNode("s:Envelope", 
                              namespaceDefinitions = c("s" = "http://www.w3.org/2003/05/soap-envelope", 
                                                       "a" = "http://www.w3.org/2005/08/addressing"))
  header <- newXMLNode("s:Header", parent=header_w_envl)
  invisible(newXMLNode("a:Action", "{{ACTION_PLACEHOLDER}}", 
                       attrs=c(`s:mustUnderstand`="1"), parent=header))
  invisible(newXMLNode("a:MessageID", paste0("urn:uuid:", UUIDgenerate()), parent=header))
  invisible(newXMLNode("a:ReplyTo", 
                       newXMLNode("a:Address", "http://www.w3.org/2005/08/addressing/anonymous"), 
                       parent=header))
  invisible(newXMLNode("a:To", 
                       urn_address,
                       attrs=c(`s:mustUnderstand`="1"),
                       parent=header))

  security <- newXMLNode("o:Security", 
                         namespaceDefinitions = c("o" = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"),
                         parent=header)
  timestamp <- newXMLNode("u:Timestamp", 
                          namespaceDefinitions = c("u" = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"),
                          attrs=c(`u:Id`="_0"), parent=security)
  invisible(newXMLNode("u:Created", 
                       #strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"),
                       created,
                       parent=timestamp))
  invisible(newXMLNode("u:Expires", 
                       #strftime(as.POSIXlt(Sys.time()+(60*60), "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"), 
                       expires,
                       parent=timestamp))
  encdata <- newXMLNode("xenc:EncryptedData",
                        attrs = c("Type"="http://www.w3.org/2001/04/xmlenc#Element"),
                        namespaceDefinitions = c("xenc" = "http://www.w3.org/2001/04/xmlenc#"), 
                        parent=security)
  invisible(newXMLNode("xenc:EncryptionMethod", 
                       attrs = c("Algorithm" = "http://www.w3.org/2001/04/xmlenc#aes256-cbc"), 
                       parent=encdata))

  keyinfo <- newXMLNode("KeyInfo", 
                        namespaceDefinitions = c("http://www.w3.org/2000/09/xmldsig#"),
                        parent=encdata)
  enckey <- newXMLNode("e:EncryptedKey", 
                       namespaceDefinitions = c("e"="http://www.w3.org/2001/04/xmlenc#"), 
                       parent=keyinfo)
  encmethod <- newXMLNode("e:EncryptionMethod",
                          attrs = c("Algorithm" = "http://www.w3.org/2001/04/xmlenc#rsa-oaep-mgf1p"), 
                          parent=enckey)
  invisible(newXMLNode("DigestMethod",
                       attrs = c("Algorithm"="http://www.w3.org/2000/09/xmldsig#sha1"), 
                       parent=encmethod))
  keyinfo2 <- newXMLNode("KeyInfo",
                         parent=enckey)
  sectokenref <- newXMLNode("o:SecurityTokenReference", 
                            namespaceDefinitions = c(o="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"), 
                            parent=keyinfo2)
  invisible(newXMLNode("X509Data",
                       newXMLNode("X509IssuerSerial", 
                                  newXMLNode("X509IssuerName", x509_issuer_name),
                                  newXMLNode("X509SerialNumber", x509_serial_number)),
                       parent = sectokenref))
  cipherdata1 <- newXMLNode("e:CipherData", parent=enckey)
  cipherval1node <- newXMLNode("e:CipherValue", ciphervalue1) 
  invisible(addChildren(cipherdata1, cipherval1node))
  invisible(setXMLNamespace(sectokenref, "o"))
  invisible(setXMLNamespace(encmethod, "e"))
  invisible(setXMLNamespace(cipherval1node, "e"))
  invisible(setXMLNamespace(cipherdata1, "e"))
  invisible(setXMLNamespace(enckey, "e"))

  invisible(newXMLNode("xenc:CipherData", 
                       newXMLNode("xenc:CipherValue", ciphervalue2), 
                       parent=encdata))
  signature <- newXMLNode("Signature",
                          namespaceDefinitions = c("http://www.w3.org/2000/09/xmldsig#"), 
                          parent=security)
  signedinfo <- newXMLNode("SignedInfo", parent = signature)
  invisible(newXMLNode("CanonicalizationMethod", 
                       attrs=c(`Algorithm`="http://www.w3.org/2001/10/xml-exc-c14n#"), 
                       parent=signedinfo))
  invisible(newXMLNode("SignatureMethod", 
                       attrs = c(`Algorithm`="http://www.w3.org/2000/09/xmldsig#hmac-sha1"), 
                       parent = signedinfo))
  reference <- newXMLNode("Reference", 
                          attrs = c(`URI`="#_0"), 
                          parent = signedinfo)

  invisible(newXMLNode("Transforms", 
                       newXMLNode("Transform",
                                  attrs=c(`Algorithm`="http://www.w3.org/2001/10/xml-exc-c14n#")), 
                       parent=reference))
  invisible(newXMLNode("DigestMethod", 
                       attrs=c(`Algorithm`="http://www.w3.org/2000/09/xmldsig#sha1"),
                       parent=reference))
  invisible(newXMLNode("DigestValue", 
                       digest_value,
                       parent=reference))
  invisible(newXMLNode("SignatureValue", signature_value, parent = signature))
  keyinfo3 <- newXMLNode("KeyInfo", parent = signature)

  sectokenref2 <- newXMLNode("o:SecurityTokenReference",
                             namespaceDefinitions = c("o" = "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"),
                             parent = keyinfo3)
  keyident <- newXMLNode("o:KeyIdentifier", 
                         key_identifier,
                         attrs = c("ValueType" = "http://docs.oasis-open.org/wss/oasis-wss-saml-token-profile-1.0#SAMLAssertionID"), 
                         parent = sectokenref2)
  invisible(setXMLNamespace(keyident, "o"))
  invisible(setXMLNamespace(sectokenref2, "o"))
  
  # set the global .state variable ---------------------------------------------
  
  .state$header <- saveXML(header_w_envl, encoding = "UTF-8", indent=FALSE)
  .state$url <- url
  .state$username <- username
  .state$password <- password
  .state$soap_path <- soap_path
  .state$urn_address <- urn_address
  .state$binary_secret <- binary_secret
  .state$key_identifier <- key_identifier
  .state$digest_value <- digest_value
  .state$signature_value <- signature_value
    
  invisible(list(header=.state$header, 
                 url=.state$url, 
                 username=.state$username, 
                 password=.state$password,
                 soap_path=.state$soap_path,
                 urn_address=.state$urn_address,
                 binary_secret=.state$binary_secret, 
                 key_identifier=.state$key_identifier,
                 digest_value=.state$digest_value,
                 signature_value=.state$signature_value))
}

#' Refresh an existing authorized MS Dynamics CRM session
#'
#' Force the current state to refresh. This is only needed for times when the token 
#' has expired
#'
#' @template verbose
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
dyn_auth_refresh <- function(verbose = FALSE){
  if(header_available(verbose)){
    dyn_auth(url=.state$url, 
             username=.state$username, 
             password=.state$password, 
             soap_path=.state$soap_path)
  } else {
    message("No authentication header found. dyn_auth_refresh() only refreshes existing states")
  }
  invisible(.state)
}

#' Check header availability
#'
#' Check if an authentication header is available in \code{\link{rdynamicscrm}}'s internal
#' \code{.state} environment.
#'
#' @return logical
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
header_available <- function(verbose = FALSE) {
  if (is.null(.state$header)) {
    if (verbose) {
      message("The header is NULL in rdynamicscrm's internal .state environment. ", 
              "This can occur if the user has not yet performed any authorization routine.\n",
              "'rdynamicscrm' will initiate re-authentication if needed. Or run dyn_auth() to trigger this explicitly.")
    }
    return(FALSE)
  }
  TRUE
}

#' Return authentication header
#'
#' @template verbose
#' @return character; a string of the header element of the current state; otherwise NULL
#' @note This function is meant to be used internally. Only use when debugging.
#' @keywords internal
#' @export
dyn_header <- function(verbose = FALSE) {
  if (!header_available(verbose = verbose)) return(NULL)
  .state$header
}
